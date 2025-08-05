// Copyright (C) 2025 Oskar Lundström

// This file is part of tiny-lsp-client.

// tiny-lsp-client is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.

// tiny-lsp-client is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.

// You should have received a copy of the GNU General Public License along with
// tiny-lsp-client. If not, see <https://www.gnu.org/licenses/>.

// @credits: This module as a whole is inspired by https://github.com/zbelial/lspce

#[cfg(test)]
mod tests;

use crate::logger;
use crate::message::*;

use serde_json::json;
use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, ErrorKind, Read, Write};
use std::ops::Drop;
use std::os::unix::process::CommandExt;
use std::process;
use std::process::{Child, ChildStdout, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{
    self, Receiver, RecvError, RecvTimeoutError, Sender, TryRecvError,
};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread;
use std::thread::{Builder, JoinHandle};
use std::time::{Duration, Instant};

pub static STOP_SERVER_ON_STDERR: AtomicBool = AtomicBool::new(false);

#[derive(Debug)]
pub struct Server {
    server_process: Arc<Mutex<Child>>,
    root_path: String,
    sender: Sender<Option<Message>>,
    receiver: Receiver<Response>,
    next_request_id: u32,
    next_version_number: isize,
    threads: Vec<JoinHandle<()>>,
}

impl Server {
    // @credits: The startup of the child process and worker threads inspired by
    // LspServer::new https://github.com/zbelial/lspce
    pub fn new(root_path: &str, server_cmd: &str) -> Option<Server> {
        let mut split = server_cmd.split(" ");
        let program = split.next().unwrap();
        let mut command = Command::new(program);
        command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .current_dir(root_path)
            // When emacs is run in a terminal (using -nw), C-g causes the LSP
            // server child process to stop. It doesn't happen when running GUI
            // emacs.  For lsp-mode, this also doesn't happen, even in termimal.
            // Based on some research, it seems like C-g causes the terminal itself
            // to send SIGINT to emacs and thus emacs' process group too. If the LSP
            // command is wrapped in a bash script that traps SIGINT (maybe SIGTERM
            // too) the LSP server is not stopped.
            .process_group(0);

        while let Some(arg) = split.next() {
            command.arg(arg);
        }

        let mut child = match command.spawn() {
            Ok(child) => child,
            Err(e) => {
                logger::log_rust_debug!(
                    "Start child failed. Reason: '{:?}'. Command: '{}'. Root path: '{}'",
                    e,
                    server_cmd,
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

        let seq_num_timestamps_recv = Arc::new(Mutex::new(BTreeMap::new()));
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
                        // todo: maybe handle don't-create-args in a more
                        // generic way also not even format! should be called if
                        // not enabled.
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
                                    lock(&seq_num_timestamps_send);
                                seq_num_timestamps.insert(id, ts);
                                while seq_num_timestamps.len() > 10 {
                                    seq_num_timestamps.pop_first();
                                }

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
            let mut reader = BufReader::new(stdout);

            loop {
                let content_header = match Self::read_header(&mut reader) {
                    Some(header) => header,
                    None => break,
                };
                let parts: Vec<&str> = content_header.split(": ").collect();
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
                    // Actualy, there are other valid header names, but handle
                    // them as they come.
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

                // Actualy, there might come other headers, but handle them
                // as they come.
                let end_of_headers = match Self::read_header(&mut reader) {
                    Some(header) => header,
                    None => break,
                };

                if end_of_headers != "" {
                    logger::log_rust_debug!(
                        "Incorrect end of headers: '{}'",
                        end_of_headers
                    );
                    break;
                }
                let mut json_buf = Vec::with_capacity(size);
                json_buf.resize(size, 0);
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

                let json_buf_clone = if logger::is_log_enabled!(LOG_RUST_DEBUG)
                {
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

                let msg: Message = match serde_json::from_str(&json) {
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
                            lock(&seq_num_timestamps_recv);
                        logger::log_rust_debug!(
                            "recv thread has '{}' timestamps: '{:?}'",
                            seq_num_timestamps.len(),
                            seq_num_timestamps
                        );

                        if let Some(ts) = seq_num_timestamps.get(&id) {
                            duration = Some(Some(ts.elapsed().as_millis()));
                            seq_num_timestamps.remove(&id);
                        } else {
                            duration = Some(None);
                        }
                        drop(seq_num_timestamps);
                    }

                    if let Ok(()) = stdout_tx.send(response) {
                    } else {
                        break;
                    }

                    // For testing purposes, the below can be used to simulate
                    // a slow server.
                    // let new_stdout_tx = stdout_tx.clone();

                    // thread::spawn(move || {
                    //     match response.result {
                    //         Some(Result::TextDocumentCompletionResult(_)) => {
                    //             std::thread::sleep(Duration::from_millis(2000))
                    //         }
                    //         _ => (),
                    //     }

                    //     if let Ok(()) = new_stdout_tx.send(response) {}
                    // });
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
                        serde_json::to_string_pretty(&full_json).unwrap();
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
                    Err(RecvError) => Err(RecvTimeoutError::Disconnected),
                };
                let mut disconnected = false;

                loop {
                    // When some data has arrived, continue to attempt
                    // reads with timeouts until it seems no more data will
                    // arrive. A timeout (of 10 ms) was needed for
                    // haskell-language-server, otherwise the STDERR lines were
                    // too frequent.
                    match result {
                        Ok(partial) => {
                            buf.extend_from_slice(&partial);
                        }
                        Err(RecvTimeoutError::Timeout) => {
                            break;
                        }
                        Err(RecvTimeoutError::Disconnected) => {
                            disconnected = true;
                            break;
                        }
                    }

                    result = stderr_rx.recv_timeout(Duration::from_millis(10));
                }

                if buf.len() > 0 {
                    let string = String::from_utf8_lossy(&buf);
                    logger::log_stderr!("{}", string);

                    if STOP_SERVER_ON_STDERR.load(Ordering::Relaxed) {
                        logger::log_rust_debug!(
                            "Stopping server due to stderr received"
                        );
                        break;
                    }
                }

                if disconnected {
                    logger::log_rust_debug!("stderr_rx disconnected");
                    break;
                }
            }

            close_thread_actions(child_stderr_thread, "stderr");
            stderr_inner_thread.join().unwrap();
        });

        Some(Server {
            server_process: child,
            root_path: root_path.to_string(),
            sender: stdin_tx,
            receiver: stdout_rx,
            next_request_id: 0,
            next_version_number: 0,
            threads: vec![send_thread, recv_thread, stderr_thread],
        })
    }

    fn read_header(reader: &mut BufReader<ChildStdout>) -> Option<String> {
        let mut buf = String::new();
        match reader.read_line(&mut buf) {
            Ok(len) => {
                if len > 0 {
                    if buf.ends_with("\r\n") {
                        buf.pop();
                        buf.pop();
                        logger::log_rust_debug!("Read header: '{}'", buf);
                        return Some(buf);
                    }
                } else {
                    logger::log_rust_debug!("recv thread got EOF");
                }
            }
            Err(e) => {
                logger::log_rust_debug!("recv thread got error '{:?}'", e);
            }
        }
        return None;
    }

    pub fn initialize(&mut self) -> Option<()> {
        let root_uri = format!("file://{}", self.root_path);

        // todo: consider what to send here, especially for completion
        let initialize_params = json!({
            "processId": process::id(),
            "rootUri": root_uri,
            "capabilities": {
                "textDocument": {
                    "definition": {
                        "linkSupport": true
                    }
                },
                "hover": {
                    "contentFormat": [
                        "plaintext"
                    ]
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
        let request = Request {
            jsonrpc: "2.0".to_string(),
            id,
            method,
            params,
        };
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
        let notification = Notification {
            jsonrpc: "2.0".to_string(),
            method,
            params,
        };
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

    // timeout: if None, no timeout, return immediately if no response.
    // Otherwise, don't return until timeout has passed.
    // Outer Option (like the other methods) represents error or not. Inner
    // Option represents wheether a response is available now or not.
    pub fn try_recv_response(
        &self,
        timeout: Option<Duration>,
    ) -> Option<Option<Response>> {
        let res = if let Some(timeout) = timeout {
            self.receiver.recv_timeout(timeout).map_err(|e| match e {
                RecvTimeoutError::Timeout => TryRecvError::Empty,
                RecvTimeoutError::Disconnected => TryRecvError::Disconnected,
            })
        } else {
            self.receiver.try_recv()
        };
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

    pub fn get_server_process_id(&self) -> u32 {
        lock(&self.server_process).id()
    }

    pub fn is_working(&mut self) -> bool {
        let result = lock(&self.server_process).try_wait();
        logger::log_rust_debug!(
            "is_working() try_wait(). Root path: '{}'. Result '{:?}'",
            self.root_path,
            result
        );
        if let Ok(None) = result {
            true
        } else {
            self.join_threads();
            false
        }
    }

    pub fn stop_server(&mut self) {
        let _ = lock(&self.server_process).kill();
        self.join_threads();
    }

    fn join_threads(&mut self) {
        let _ = self.sender.send(None);
        for thread in std::mem::take(&mut self.threads) {
            let thread_name = thread.thread().name().map(|s| s.to_string());
            logger::log_rust_debug!("Waiting for '{:?}'", thread_name);
            thread.join().unwrap();
            logger::log_rust_debug!("Done waiting for '{:?}'", thread_name);
        }
    }
}

impl Drop for Server {
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

fn close_thread_actions(server_process: Arc<Mutex<Child>>, thread_name: &str) {
    logger::log_rust_debug!("{} thread stopping server", thread_name);
    let result = lock(&server_process).kill();
    logger::log_rust_debug!(
        "{} thread stop server result '{:?}'",
        thread_name,
        result
    );
    logger::log_rust_debug!("{} thread closing", thread_name);
}

// The design of this module is that threads should never crash, and if they do,
// it indicates a big problem so that everything should crash.
// Maybe too offensive coding, but let's see how it turns out in practice.
fn lock<T>(mutex: &Mutex<T>) -> MutexGuard<T> {
    mutex.lock().unwrap()
}
