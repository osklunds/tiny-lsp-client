// Heavily inspired by https://github.com/zbelial/lspce

#![allow(warnings)] 

use std::process::{Command, Stdio, Child, ChildStdout};
use std::io::Read;
use std::io::Write;
use serde_json::{json, Value, Number};
use serde::Deserialize;
use serde::Serialize;
use std::thread;
use std::time::Duration;
use std::io::{BufRead, BufReader};
use std::sync::mpsc::{self, Sender, Receiver};

mod learning_tests;
mod dummy;

fn main() {

}

#[derive(Debug, Serialize, Deserialize, Clone)]
enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification)
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Request {
    pub id: u32,
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Response {
    pub id: u32,
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Notification {
    pub method: String,
    pub params: serde_json::Value,
}

struct Connection {
    server_process: Child,
    root_uri: String,
    sender: Sender<Message>,
    receiver: Receiver<Message>
}

impl Connection {
    pub fn new(
        command: &str,
        root_path: &str,
    ) -> Connection {
        let mut child = Command::new(command)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn().unwrap();

        let mut stdin = child.stdin.take().unwrap();
        let mut stdout = child.stdout.take().unwrap();
        let mut stderr = child.stderr.take().unwrap();

        let (stdin_tx, stdin_rx) = mpsc::channel();

        thread::spawn(move || {
            loop {
                if let Ok(msg) = stdin_rx.recv() {
                    send(&msg, &mut stdin);
                } else {
                    return;
                }
            }
        });

        let (stdout_tx, stdout_rx) = mpsc::channel();
        let mut reader = std::io::BufReader::new(stdout);

        thread::spawn(move || {
            loop {
                let msg = recv(&mut reader);
                if let Ok(()) = stdout_tx.send(msg) {

                } else {
                    return;
                }
            }
        });

        thread::spawn(move || {
            loop {
                let mut buf = [0; 100];
                let len = stderr.read(&mut buf).unwrap();
                println!("oskar stderr: {} {}",
                         len,
                         String::from_utf8(buf.to_vec()).unwrap()
                );
            }
        });

        let root_uri = format!("file:///{}", root_path);

        let initialize_params = json!({
            "processId": null,
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

        let initialize_request = Request {
            id: 0,
            method: "initialize".to_string(),
            params: initialize_params
        };
        let initialize_request = Message::Request(initialize_request);
        stdin_tx.send(initialize_request).unwrap();

        let initialize_response = stdout_rx.recv().unwrap();

        let initialized_notification = Notification {
            method: "initialized".to_string(),
            params: json!({})
        };
        let initialized_notification = Message::Notification(initialized_notification);
        
        stdin_tx.send(initialized_notification).unwrap();

        Connection {
            server_process: child,
            root_uri: root_uri.to_string(),
            sender: stdin_tx,
            receiver: stdout_rx
        }
    }
}

fn send_json<W: Write>(json: &str, writer: &mut W) {
    let full = format!("Content-Length: {}\r\n\r\n{}", json.len(), &json);
    // println!("oskar sending: {:?}", full);
    writer.write(full.as_bytes()).unwrap();
}

fn send<W: Write>(msg: &Message, writer: &mut W) {
    let json = serde_json::to_string(&msg).unwrap();
    send_json(&json, writer);
}

fn recv(reader: &mut BufReader<ChildStdout>) -> Message {
    let mut buf = String::new();
    reader.read_line(&mut buf).unwrap();
    buf.pop();
    buf.pop();
    let parts: Vec<&str> = buf.split(": ").collect();
    let header_name = parts[0];
    let header_value = parts[1];

    let size = header_value.parse::<usize>().unwrap();
    // println!("oskar: {:?}", size);

    let mut json_buf = Vec::new();
    // todo: +2 might be related the the pops above. Anyway,
    // need to find out why
    json_buf.resize(size+2, 0);
    reader.read_exact(&mut json_buf);
    // println!("oskar: {:?}", json_buf);
    let json = String::from_utf8(json_buf).unwrap();
    // println!("oskar: {}", json);

    let response = serde_json::from_str(&json).unwrap();
    Message::Response(response)
}
