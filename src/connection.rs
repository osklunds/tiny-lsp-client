#[cfg(test)]
mod tests;

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
use std::time::Duration;

pub struct Connection {
    server_process: Child,
    root_path: String,
    sender: Sender<Message>,
    receiver: Receiver<Message>,
    next_request_id: u32,
}

impl Connection {
    pub fn new(command: &str, root_path: &str) -> Connection {
        let mut child = Command::new(command)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();

        let mut stdin = child.stdin.take().unwrap();
        let mut stdout = child.stdout.take().unwrap();
        let mut stderr = child.stderr.take().unwrap();

        let (stdin_tx, stdin_rx) = mpsc::channel();

        // Receiver of messages from application
        // Sending over stdin to lsp server
        thread::spawn(move || loop {
            if let Ok(msg) = stdin_rx.recv() {
                let json = serde_json::to_string(&msg).unwrap();
                let full =
                    format!("Content-Length: {}\r\n\r\n{}", json.len(), &json);
                println!("Sent: {}", full);
                stdin.write(full.as_bytes()).unwrap();
            } else {
                return;
            }
        });

        let (stdout_tx, stdout_rx) = mpsc::channel();
        // Sender of messages to application
        // Receiving from stdout from lsp server
        thread::spawn(move || {
            let mut reader = std::io::BufReader::new(stdout);

            loop {
                let mut buf = String::new();
                reader.read_line(&mut buf).unwrap();
                buf.pop();
                buf.pop();
                // println!("oskar2: {:?}", buf);
                let parts: Vec<&str> = buf.split(": ").collect();
                let header_name = parts[0];
                let header_value = parts[1];

                let size = header_value.parse::<usize>().unwrap();
                // println!("oskar: {:?}", size);

                let mut json_buf = Vec::new();
                // todo: +2 might be related the the pops above. Anyway,
                // need to find out why
                json_buf.resize(size + 2, 0);
                reader.read_exact(&mut json_buf);
                // println!("oskar3: {:?}", json_buf);
                let json = String::from_utf8(json_buf).unwrap();
                println!("Received: {}", json);
                thread::sleep(Duration::from_millis(100));

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

        thread::spawn(move || loop {
            let mut buf = [0; 500];
            let len = stderr.read(&mut buf).unwrap();
            println!(
                "oskar stderr: {} {}",
                len,
                String::from_utf8(buf.to_vec()).unwrap()
            );

            if len == 0 {
                return;
            }
        });

        Connection {
            server_process: child,
            root_path: root_path.to_string(),
            sender: stdin_tx,
            receiver: stdout_rx,
            next_request_id: 0,
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
}
