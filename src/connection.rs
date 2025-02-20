#[cfg(test)]
mod tests;

use crate::logger;
use crate::message::*;

use serde_json::json;
use std::io::BufRead;
use std::io::ErrorKind;
use std::io::Read;
use std::io::Write;
use std::ops::Drop;
use std::process;
use std::process::{Child, Command, Stdio};
use std::sync::atomic::Ordering;
use std::sync::mpsc::{self, Receiver, RecvError, Sender, TryRecvError};
use std::sync::{Arc, Mutex};
use std::thread;
use std::thread::{Builder, JoinHandle};
use std::time::{Duration, Instant};

pub struct Connection {
    server_process: Arc<Mutex<Child>>,
    command: String,
    root_path: String,
    sender: Sender<Option<Message>>,
    receiver: Receiver<Response>,
    next_request_id: u32,
    next_version_number: isize,
    threads: Vec<JoinHandle<()>>,
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
                logger::log_rust_debug!(
                    "Start child failed. Reason: '{:?}'. Command: '{}'. Root path: '{}'",
                    e,
                    command,
                    root_path
                );
                return None;
            }
        };
        let mut stdin = child.stdin.take().unwrap();
        let stdout = child.stdout.take().unwrap();
        let mut stderr = child.stderr.take().unwrap();

        let child = Mutex::new(child);
        let child = Arc::new(child);

        let (stdin_tx, stdin_rx) = mpsc::channel();
        let child_send_thread = Arc::clone(&child);

        let seq_num_timestamps_recv = Arc::new(Mutex::new(Vec::new()));
        let seq_num_timestamps_send = Arc::clone(&seq_num_timestamps_recv);

        // Receiver of messages from application
        // Sending over stdin to lsp server
        let send_thread = spawn_named_thread("send", move || {
            loop {
                if let Ok(maybe_msg) = stdin_rx.recv() {
                    if let Some(msg) = maybe_msg {
                        let json = serde_json::to_string(&msg).unwrap();
                        let full = format!(
                            "Content-Length: {}\r\n\r\n{}",
                            json.len(),
                            &json
                        );

                        match stdin.write_all(full.as_bytes()) {
                            Ok(()) => (),
                            Err(e) => {
                                logger::log_rust_debug!(
                                "Error writing to stdin. Reason: '{:?}'. Data: '{}'.",
                                e,
                                full,
                            );
                                break;
                            }
                        }

                        // Don't create pretty json if logging is not enabled
                        // todo: maybe handle don't-create-args in a more generic way
                        if logger::is_log_enabled!(LOG_IO) {
                            logger::log_io!(
                                "Sent: {}",
                                serde_json::to_string_pretty(&msg).unwrap()
                            );
                        }

                        // Only touch the mutex if IO is logged
                        if logger::is_log_enabled!(LOG_IO) {
                            if let Message::Request(request) = msg {
                                let id = request.id;
                                let ts = Instant::now();
                                let mut seq_num_timestamps =
                                    match seq_num_timestamps_send.lock() {
                                        Ok(locked) => locked,
                                        Err(_) => {
                                            logger::log_rust_debug!(
                                            "seq_num_timestamps lock failed in send thread"
                                        );
                                            break;
                                        }
                                    };
                                seq_num_timestamps.push((id, ts));
                                seq_num_timestamps.truncate(10);

                                logger::log_rust_debug!(
                                    "send thread has '{}' timestamps: '{:?}'",
                                    seq_num_timestamps.len(),
                                    seq_num_timestamps
                                );
                            }
                        }
                    } else {
                        break;
                    }
                } else {
                    logger::log_rust_debug!("send thread got None from channel. Closing early intentionally.");
                    break;
                }
            }

            close_thread_actions(child_send_thread, "send");
        });

        let (stdout_tx, stdout_rx) = mpsc::channel();
        let child_recv_thread = Arc::clone(&child);

        // Sender of messages to application
        // Receiving from stdout from lsp server
        let recv_thread = spawn_named_thread("recv", move || {
            let mut reader = std::io::BufReader::new(stdout);

            loop {
                let mut buf = String::new();
                match reader.read_line(&mut buf) {
                    Ok(len) => {
                        if len > 0 {
                            logger::log_rust_debug!(
                                "Connection recv loop initial line: '{:?}'",
                                buf
                            );
                            buf.pop();
                            buf.pop();
                            let parts: Vec<&str> = buf.split(": ").collect();
                            let num_parts = parts.len();
                            if num_parts != 2 {
                                logger::log_rust_debug!(
                                    "Incorrect number of parts after split: '{}'. Parts: '{:?}'",
                                    num_parts,
                                    parts
                                );
                                break;
                            }

                            let header_name = parts[0];
                            if header_name != "Content-Length" {
                                // Actualy, there are other valid header names,
                                // but handle them as they come.
                                logger::log_rust_debug!(
                                    "Incorrect header name: '{}'",
                                    header_name
                                );
                                break;
                            }
                            let header_value = parts[1];

                            let size = match header_value.parse::<usize>() {
                                Ok(s) => s,
                                Err(e) => {
                                    logger::log_rust_debug!(
                                        "Could not parse size: '{:?}'",
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
                                    logger::log_rust_debug!(
                                        "read_exact of json failed: {:?}",
                                        e
                                    );
                                    break;
                                }
                            }

                            let json_buf_clone =
                                if logger::is_log_enabled!(LOG_RUST_DEBUG) {
                                    Some(json_buf.clone())
                                } else {
                                    None
                                };
                            let json = match String::from_utf8(json_buf) {
                                Ok(json) => json,
                                Err(e) => {
                                    let json_buf_clone =
                                        json_buf_clone.unwrap_or("".into());

                                    logger::log_rust_debug!(
                                        "from_utf8 failed. Reason: '{:?}'. Data as utf8 lossy: '{}'. Raw bytes: '{:?}'",
                                        e,
                                        String::from_utf8_lossy(&json_buf_clone),
                                        json_buf_clone
                                    );
                                    break;
                                }
                            };

                            let msg: Message = match serde_json::from_str(&json)
                            {
                                Ok(msg) => msg,
                                Err(e) => {
                                    logger::log_rust_debug!(
                                        "serde_json::from_str failed: '{:?}'",
                                        e
                                    );
                                    break;
                                }
                            };

                            let mut duration = None;

                            // Only care about response so far, i.e. drop
                            // notifications about e.g. diagnostics
                            if let Message::Response(response) = msg {
                                let id = response.id;

                                // Only touch the mutex if IO is logged
                                if logger::is_log_enabled!(LOG_IO) {
                                    let mut seq_num_timestamps =
                                        match seq_num_timestamps_recv.lock() {
                                            Ok(locked) => locked,
                                            Err(_) => {
                                                logger::log_rust_debug!(
                                                    "seq_num_timestamps lock failed in recv thread"
                                                );
                                                break;
                                            }
                                        };
                                    logger::log_rust_debug!(
                                        "recv thread has '{}' timestamps: '{:?}'",
                                        seq_num_timestamps.len(),
                                        seq_num_timestamps
                                    );
                                    let lookup_result = seq_num_timestamps
                                        .iter()
                                        .enumerate()
                                        .find(|(_i, (curr_id, _ts))| {
                                            *curr_id == id
                                        });
                                    if let Some((index, (_id, ts))) =
                                        lookup_result
                                    {
                                        duration = Some(Some(
                                            ts.elapsed().as_millis(),
                                        ));
                                        seq_num_timestamps.swap_remove(index);
                                    } else {
                                        duration = Some(None);
                                    }
                                    drop(seq_num_timestamps);
                                }

                                if let Ok(()) = stdout_tx.send(response) {
                                } else {
                                    break;
                                }
                            }

                            if logger::is_log_enabled!(LOG_IO) {
                                // Decode as serde_json::Value too, to be able
                                // to print fields not deserialized into msg.
                                let full_json: serde_json::Value =
                                    match serde_json::from_str(&json) {
                                        Ok(full_json) => full_json,
                                        Err(e) => {
                                            logger::log_rust_debug!(
                                                "Parse to full json failed. Reason: '{:?}'. Data: '{}'.",
                                                e,
                                                json
                                            );
                                            break;
                                        }
                                    };
                                let pretty_json =
                                    serde_json::to_string_pretty(&full_json)
                                        .unwrap();
                                let duration_part = match duration {
                                    // Response to request where time could be found
                                    Some(Some(ms)) => format!("({} ms) ", ms),

                                    // Response to request where time could not be found
                                    Some(None) => format!("(? ms) "),

                                    // Notification
                                    None => format!(""),
                                };
                                logger::log_io!(
                                    "Received: {}{}",
                                    duration_part,
                                    pretty_json
                                );
                            }
                        } else {
                            logger::log_rust_debug!("recv thread got EOF");
                            break;
                        }
                    }
                    Err(e) => {
                        logger::log_rust_debug!(
                            "recv thread got error '{:?}'",
                            e
                        );
                        break;
                    }
                }
            }

            close_thread_actions(child_recv_thread, "recv");
        });

        let child_stderr_thread = Arc::clone(&child);
        let stderr_thread = spawn_named_thread("stderr", move || {
            let (stderr_tx, stderr_rx) = mpsc::channel();
            let stderr_inner_thread = spawn_named_thread(
                "stderr_inner",
                move || {
                    loop {
                        let mut buf = [0; 500];
                        match stderr.read(&mut buf) {
                            Ok(len) => {
                                if len > 0 {
                                    if let Err(e) =
                                        stderr_tx.send(buf[0..len].to_vec())
                                    {
                                        logger::log_rust_debug!(
                                    "stderr_inner got error when sending to stderr. Reason: '{:?}'",
                                    e
                                );
                                        break;
                                    }
                                } else {
                                    logger::log_rust_debug!(
                                        "stderr_inner got EOF"
                                    );
                                    break;
                                }
                            }
                            Err(e) => {
                                if e.kind() == ErrorKind::Interrupted {
                                    // Continue to loop
                                    // This happens too often to log. Feels almost like
                                    // a normal case. Do a short sleep to avoid busy
                                    // looping. Not too long sleep because then can miss
                                    // to print things in case other parts close down
                                    // quicker
                                    thread::sleep(Duration::from_micros(100));
                                } else {
                                    logger::log_rust_debug!(
                                "stderr_inner got error when reading. Reason: '{:?}'",
                                e
                            );
                                    break;
                                }
                            }
                        }
                    }
                    logger::log_rust_debug!("stderr_inner closing");
                },
            );

            loop {
                let mut buf = Vec::new();

                // First do a blocking read until some data arrives. No point
                // in doing non-blocking yet.
                let mut result = match stderr_rx.recv() {
                    Ok(r) => Ok(r),
                    Err(RecvError) => Err(TryRecvError::Disconnected),
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
                    let string = String::from_utf8_lossy(&buf);
                    logger::log_stderr!("{}", string);
                }

                if disconnected {
                    logger::log_rust_debug!("stderr_rx disconnected");
                    break;
                }
            }

            stderr_inner_thread.join().unwrap();
            close_thread_actions(child_stderr_thread, "stderr");
        });

        Some(Connection {
            server_process: child,
            command: command.to_string(),
            root_path: root_path.to_string(),
            sender: stdin_tx,
            receiver: stdout_rx,
            next_request_id: 0,
            next_version_number: 0,
            threads: vec![send_thread, recv_thread, stderr_thread],
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
        match self.sender.send(Some(Message::Request(request))) {
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
        match self.sender.send(Some(Message::Notification(notification))) {
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
        // todo: avoid unwrap on server_process
        self.server_process.lock().unwrap().id()
    }

    pub fn is_working(&self) -> bool {
        let result = self.server_process.lock().unwrap().try_wait();
        logger::log_rust_debug!(
            "is_working() try_wait(). Root path: '{}'. Result '{:?}'",
            self.root_path,
            result
        );
        if let Ok(None) = result {
            true
        } else {
            false
        }
    }

    pub fn stop_server(&mut self) {
        let _ = self.server_process.lock().unwrap().kill();
        let _ = self.sender.send(None);
        for thread in std::mem::take(&mut self.threads) {
            thread.join().unwrap();
        }
    }
}

impl Drop for Connection {
    fn drop(&mut self) {
        self.stop_server();
    }
}

fn spawn_named_thread<F, T, N: AsRef<str>>(name: N, f: F) -> JoinHandle<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    Builder::new()
        .name(name.as_ref().to_string())
        .spawn(f)
        .unwrap()
}

fn close_thread_actions(child: Arc<Mutex<Child>>, thread_name: &str) {
    logger::log_rust_debug!("{} thread stopping server", thread_name);
    if let Ok(mut locked) = child.lock() {
        let result = locked.kill();
        logger::log_rust_debug!(
            "{} thread stop server result '{:?}'",
            thread_name,
            result
        );
    } else {
        logger::log_rust_debug!("{} thread lock child failed", thread_name);
    }
    logger::log_rust_debug!("{} thread closing", thread_name);
}
