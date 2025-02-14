#[cfg(test)]
mod tests;

use crate::logger;
use crate::message::*;

use serde::Deserialize;
use serde::Serialize;
use serde_json::{json, Number, Value};
use std::io;
use std::io::Read;
use std::io::Write;
use std::io::{BufRead, BufReader};
use std::process;
use std::process::{Child, ChildStdout, Command, Stdio};
use std::sync::atomic::Ordering;
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::sync::{Arc, Mutex};
use std::thread;
use std::thread::{Builder, JoinHandle};
use std::time::Duration;

pub struct Connection {
    server_process: Arc<Mutex<Child>>,
    command: String,
    root_path: String,
    sender: Sender<Message>,
    receiver: Receiver<Response>,
    next_request_id: u32,
    next_version_number: isize,
}

impl Connection {
    pub fn new(command: &str, root_path: &str) -> Option<Connection> {
        let mut child = match Command::new(command)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .current_dir(root_path)
            .spawn()
        {
            Ok(child) => child,
            Err(e) => {
                logger::log_debug!("start child failed: {:?}", e);
                return None;
            }
        };
        let mut stdin = child.stdin.take().unwrap();
        let mut stdout = child.stdout.take().unwrap();
        let mut stderr = child.stderr.take().unwrap();

        let child = Mutex::new(child);
        let child = Arc::new(child);

        let (stdin_tx, stdin_rx) = mpsc::channel();
        let child_send_thread = Arc::clone(&child);

        // Receiver of messages from application
        // Sending over stdin to lsp server
        spawn_named_thread("send", move || {
            loop {
                if let Ok(msg) = stdin_rx.recv() {
                    let json = serde_json::to_string(&msg).unwrap();
                    let full = format!(
                        "Content-Length: {}\r\n\r\n{}",
                        json.len(),
                        &json
                    );

                    // todo: stdin closed isn't noticed until try to send. See if
                    // that can be improved
                    match stdin.write_all(full.as_bytes()) {
                        Ok(()) => (),
                        Err(e) => {
                            logger::log_debug!(
                                "Error writing {} to stdin {:?}",
                                full,
                                e
                            );
                            break;
                        }
                    }
                    logger::log_io!(
                        "Sent: {}",
                        serde_json::to_string_pretty(&msg).unwrap()
                    );
                } else {
                    break;
                }
            }

            logger::log_debug!("send thread killing server");
            child_send_thread.lock().unwrap().kill();
        });

        let (stdout_tx, stdout_rx) = mpsc::channel();
        let child_recv_thread = Arc::clone(&child);

        // Sender of messages to application
        // Receiving from stdout from lsp server
        spawn_named_thread("recv", move || {
            let mut reader = std::io::BufReader::new(stdout);

            loop {
                let mut buf = String::new();
                match reader.read_line(&mut buf) {
                    Ok(len) => {
                        if len > 0 {
                            logger::log_debug!(
                                "Connection recv loop initial line: {:?}",
                                buf
                            );
                            buf.pop();
                            buf.pop();
                            let parts: Vec<&str> = buf.split(": ").collect();
                            let num_parts = parts.len();
                            if num_parts != 2 {
                                logger::log_debug!(
                                    "Incorrect number of parts after split: {}",
                                    num_parts
                                );
                                break;
                            }

                            let header_name = parts[0];
                            if header_name != "Content-Length" {
                                // Actualy, there are other valid header names,
                                // but handle them as they come.
                                logger::log_debug!(
                                    "Incorrect header name: {}",
                                    header_name
                                );
                                break;
                            }
                            let header_value = parts[1];

                            let size = match header_value.parse::<usize>() {
                                Ok(s) => s,
                                Err(e) => {
                                    logger::log_debug!(
                                        "Could not parse size: {:?}",
                                        e
                                    );
                                    break;
                                }
                            };

                            let mut json_buf = Vec::new();
                            // todo: +2 might be related the the pops above. Anyway,
                            // need to find out why
                            json_buf.resize(size + 2, 0);
                            match reader.read_exact(&mut json_buf) {
                                Ok(()) => (),
                                Err(e) => {
                                    logger::log_debug!(
                                        "read_exact of json failed: {:?}",
                                        e
                                    );
                                    break;
                                }
                            }

                            let json = match String::from_utf8(json_buf) {
                                Ok(json) => json,
                                Err(e) => {
                                    logger::log_debug!(
                                        "from_utf8 failed: {:?}",
                                        e
                                    );
                                    break;
                                }
                            };

                            if logger::LOG_IO.load(Ordering::Relaxed) {
                                // Decode as serde_json::Value too, to be able
                                // to print fields not deserialized into msg.
                                let full_json: serde_json::Value =
                                    match serde_json::from_str(&json) {
                                        Ok(full_json) => full_json,
                                        Err(e) => {
                                            logger::log_debug!(
                                                "Parse to full json failed: {:?} reason: {:?}",
                                                json,
                                                e
                                            );
                                            break;
                                        }
                                    };
                                logger::log_io!(
                                    "Received: {}",
                                    serde_json::to_string_pretty(&full_json)
                                        .unwrap()
                                );
                            }

                            let msg = match serde_json::from_str(&json) {
                                Ok(msg) => msg,
                                Err(e) => {
                                    logger::log_debug!(
                                        "serde_json::from_str failed: {:?}",
                                        e
                                    );
                                    break;
                                }
                            };

                            // Only care about response so far, i.e. drop notifications
                            // about e.g. diagnostics
                            if let Message::Response(response) = msg {
                                if let Ok(()) = stdout_tx.send(response) {
                                } else {
                                    break;
                                }
                            }
                        } else {
                            logger::log_debug!("stdio got EOF");
                            break;
                        }
                    }
                    Err(e) => {
                        logger::log_debug!("stdio got error {:?}", e);
                        break;
                    }
                }
            }

            // todo: the intention was that if recv thread sees channel closed,
            // it means we should kill the server, and then send thead closes
            // automatically and detect before next request is sent. But
            // of course, the send thread is still blocked on send channel
            // so still too late. So maybe revert Arc<Mutex> and check is_working
            // when calling request() etc. None or is_working() = false both have
            // the same semantics, but None means it happened while handling
            // something.
            logger::log_debug!("recv thread killing server");
            child_recv_thread.lock().unwrap().kill();
        });

        let child_stderr_thread = Arc::clone(&child);
        spawn_named_thread("stderr", move || {
            let (stderr_tx, stderr_rx) = mpsc::channel();
            spawn_named_thread("stderr_inner", move || loop {
                let mut buf = [0; 500];
                match stderr.read(&mut buf) {
                    Ok(len) => {
                        if len > 0 {
                            stderr_tx.send(buf[0..len].to_vec());
                        } else {
                            logger::log_debug!("stderr_inner got EOF");
                            break;
                        }
                    }
                    Err(e) => {
                        logger::log_debug!("stderr_inner got error {:?}", e);
                        break;
                    }
                }
            });

            loop {
                let mut buf = Vec::new();

                // First do a blocking read until some data arrives. No point
                // in doing non-blocking yet.
                let mut result = match stderr_rx.recv() {
                    Ok(r) => Ok(r),
                    RecvError => Err(TryRecvError::Disconnected),
                };
                let mut disconnected = false;

                loop {
                    // When some data has arrived, continue to attempt
                    // non-blocking reads until it seems no more data will
                    // arrive.
                    match result {
                        Ok(partial) => {
                            buf.extend_from_slice(&partial);
                        }
                        Err(TryRecvError::Empty) => {
                            break;
                        }
                        Err(TryRecvError::Disconnected) => {
                            disconnected = true;
                            break;
                        }
                    }

                    result = stderr_rx.try_recv();
                }

                if buf.len() > 0 {
                    match String::from_utf8(buf.clone()) {
                        Ok(string) => {
                            logger::log_stderr!("{}", string);
                        }
                        Err(e) => {
                            logger::log_stderr!("{:?}", buf);
                            logger::log_debug!(
                                "from_utf8 failed for stderr {:?} reason: {:?}",
                                buf,
                                e
                            );
                            break;
                        }
                    }
                }

                if disconnected {
                    logger::log_debug!("stderr_rx disconnected");
                    break;
                }
            }

            logger::log_debug!("stderr thread killing server");
            child_stderr_thread.lock().unwrap().kill();
        });

        Some(Connection {
            server_process: child,
            command: command.to_string(),
            root_path: root_path.to_string(),
            sender: stdin_tx,
            receiver: stdout_rx,
            next_request_id: 0,
            next_version_number: 0,
        })
    }

    pub fn initialize(&mut self) -> Option<()> {
        let root_uri = format!("file://{}", self.root_path);

        let initialize_params = json!({
            "processId": process::id(),
            "rootUri": root_uri,
            "capabilities": {
                "textDocument": {
                    "definition": {
                        "dynamicRegistration": true,
                        "linkSupport": true
                    }
                }
            }
        });
        self.send_request(
            "initialize".to_string(),
            RequestParams::Untyped(initialize_params),
        )?;

        self.recv_response()?;

        self.send_notification(
            "initialized".to_string(),
            NotificationParams::Untyped(json!({})),
        )?;

        Some(())
    }

    // all these should return Option. If None, then lib should close server
    // and prompt user. If user says no, then disable mode
    pub fn send_request(
        &mut self,
        method: String,
        params: RequestParams,
    ) -> Option<u32> {
        let id = self.next_request_id;
        self.next_request_id += 1;
        let request = Request { id, method, params };
        match self.sender.send(Message::Request(request)) {
            Ok(()) => Some(id),
            Err(_) => None,
        }
    }

    pub fn send_notification(
        &self,
        method: String,
        params: NotificationParams,
    ) -> Option<()> {
        let notification = Notification { method, params };
        match self.sender.send(Message::Notification(notification)) {
            Ok(()) => Some(()),
            Err(_) => None,
        }
    }

    pub fn recv_response(&self) -> Option<Response> {
        if let Ok(response) = self.receiver.recv() {
            Some(response)
        } else {
            None
        }
    }

    // todo: inner means result, outer means if error. Try to improve
    pub fn try_recv_response(&self) -> Option<Option<Response>> {
        let res = self.receiver.try_recv();
        if let Ok(response) = res {
            Some(Some(response))
        } else if let Err(TryRecvError::Empty) = res {
            Some(None)
        } else {
            None
        }
    }

    pub fn inc_and_get_version_number(&mut self) -> isize {
        let version_number = self.next_version_number;
        self.next_version_number += 1;
        version_number
    }

    pub fn get_command(&self) -> String {
        self.command.clone()
    }

    pub fn get_server_process_id(&self) -> u32 {
        self.server_process.lock().unwrap().id()
    }

    pub fn is_working(&self) -> bool {
        let result = self.server_process.lock().unwrap().try_wait();
        logger::log_debug!("try_wait result {:?}", result);
        if let Ok(None) = result {
            true
        } else {
            false
        }
    }

    pub fn stop_server(&self) {
        self.server_process.lock().unwrap().kill();
    }
}

fn spawn_named_thread<F, T, N: AsRef<str>>(
    name: N,
    f: F,
) -> std::io::Result<JoinHandle<T>>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    Builder::new().name(name.as_ref().to_string()).spawn(f)
}
