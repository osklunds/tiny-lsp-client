#[cfg(test)]
mod tests;

use crate::logger;
use crate::message::*;

use serde::Deserialize;
use serde::Serialize;
use serde_json::{json, Number, Value};
use std::io::Read;
use std::io::Write;
use std::io::{BufRead, BufReader};
use std::process;
use std::process::{Child, ChildStdout, Command, Stdio};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread;
use std::thread::{Builder, JoinHandle};
use std::time::Duration;

pub struct Connection {
    server_process: Child,
    command: String,
    root_path: String,
    sender: Sender<Message>,
    receiver: Receiver<Message>,
    next_request_id: u32,
    next_version_number: isize,
}

impl Connection {
    pub fn new(command: &str, root_path: &str) -> Connection {
        let mut child = Command::new(command)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .current_dir(root_path)
            .spawn()
            .unwrap();

        let mut stdin = child.stdin.take().unwrap();
        let mut stdout = child.stdout.take().unwrap();
        let mut stderr = child.stderr.take().unwrap();

        let (stdin_tx, stdin_rx) = mpsc::channel();

        // Receiver of messages from application
        // Sending over stdin to lsp server
        spawn_named_thread("send", move || loop {
            if let Ok(msg) = stdin_rx.recv() {
                let json = serde_json::to_string(&msg).unwrap();
                let full =
                    format!("Content-Length: {}\r\n\r\n{}", json.len(), &json);
                stdin.write(full.as_bytes()).unwrap();
                logger::log_io!(
                    "Sent: {}",
                    serde_json::to_string_pretty(&msg).unwrap()
                );
            } else {
                return;
            }
        });

        let (stdout_tx, stdout_rx) = mpsc::channel();
        // Sender of messages to application
        // Receiving from stdout from lsp server
        spawn_named_thread("recv", move || {
            let mut reader = std::io::BufReader::new(stdout);

            loop {
                let mut buf = String::new();
                reader.read_line(&mut buf).unwrap();
                logger::log_debug!(
                    "Connection recv loop initial line: {:?}",
                    buf
                );
                buf.pop();
                buf.pop();
                let parts: Vec<&str> = buf.split(": ").collect();
                if parts.len() < 2 {
                    logger::log_debug!("Note: strange header");
                    continue;
                }

                let header_name = parts[0];
                let header_value = parts[1];

                let size = header_value.parse::<usize>().unwrap();

                let mut json_buf = Vec::new();
                // todo: +2 might be related the the pops above. Anyway,
                // need to find out why
                json_buf.resize(size + 2, 0);
                reader.read_exact(&mut json_buf);
                let json = String::from_utf8(json_buf).unwrap();

                // Decode as serde_json::Value too, to be able to print fields
                // not deserialized into msg.
                let full_json: serde_json::Value =
                    serde_json::from_str(&json).unwrap();
                logger::log_io!(
                    "Received: {}",
                    serde_json::to_string_pretty(&full_json).unwrap()
                );

                let msg = serde_json::from_str(&json).unwrap();

                // Only care about response so far, i.e. drop notifications
                // about e.g. diagnostics
                if let Message::Response(_) = msg {
                    if let Ok(()) = stdout_tx.send(msg) {
                    } else {
                        return;
                    }
                }
            }
        });

        spawn_named_thread("stderr", move || {
            let (stderr_tx, stderr_rx) = mpsc::channel();
            spawn_named_thread("stderr_inner", move || loop {
                let mut buf = [0; 500];
                let len = stderr.read(&mut buf).unwrap();
                stderr_tx.send(buf[0..len].to_vec());
            });

            loop {
                let mut buf = Vec::new();
                // todo: see if can make the first blocking read less duplicated
                // and more elegant.

                // First do a blocking read until some data arrives. No point
                // in doing non-blocking yet.
                let partial = stderr_rx.recv().unwrap();
                buf.extend_from_slice(&partial);

                loop {
                    // When some data has arrived, continue to attempt
                    // non-blocking reads until it seems no more data will
                    // arrive.
                    match stderr_rx.recv_timeout(Duration::from_millis(1)) {
                        Ok(partial) => {
                            buf.extend_from_slice(&partial);
                        }
                        Err(mpsc::RecvTimeoutError::Timeout) => {
                            break;
                        }
                        Err(mpsc::RecvTimeoutError::Disconnected) => {
                            panic!();
                        }
                    }
                }

                logger::log_stderr!("{}", String::from_utf8(buf).unwrap());
            }
        });

        Connection {
            server_process: child,
            command: command.to_string(),
            root_path: root_path.to_string(),
            sender: stdin_tx,
            receiver: stdout_rx,
            next_request_id: 0,
            next_version_number: 0,
        }
    }

    pub fn initialize(&mut self) {
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
        );

        let _initialize_response = self.recv_response();

        self.send_notification(
            "initialized".to_string(),
            NotificationParams::Untyped(json!({})),
        );
    }

    pub fn send_request(
        &mut self,
        method: String,
        params: RequestParams,
    ) -> u32 {
        let id = self.next_request_id;
        self.next_request_id += 1;
        let request = Request { id, method, params };
        self.sender.send(Message::Request(request)).unwrap();
        id
    }

    pub fn send_notification(
        &self,
        method: String,
        params: NotificationParams,
    ) {
        let notification = Notification { method, params };
        self.sender
            .send(Message::Notification(notification))
            .unwrap();
    }

    pub fn recv_response(&self) -> Response {
        if let Message::Response(response) = self.receiver.recv().unwrap() {
            response
        } else {
            panic!("hej")
        }
    }

    pub fn try_recv_response(&self) -> Option<Response> {
        let res = self.receiver.try_recv();
        if let Ok(Message::Response(response)) = res {
            Some(response)
        } else if let Err(TryRecvError::Empty) = res {
            None
        } else {
            panic!("hej")
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
        self.server_process.id()
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
