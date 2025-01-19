
#![allow(warnings)] 

use std::process::{Command, Stdio};
use std::io::Read;
use std::io::Write;
use serde_json::json;
use serde::Deserialize;
use serde::Serialize;

fn main2() {
    println!("Hello, world!");

    let mut child = Command::new("rust-analyzer")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn().unwrap();

    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();

    let res = stdin.write(b"hejhej\n");
    println!("oskar stdin: {:?}", res);

    let mut res = Vec::new();
    stderr.read_to_end(&mut res);
    println!("oskar stderr: {}", String::from_utf8(res).unwrap());

    other_fun();
}

fn other_fun() {
    println!("oskar: {:?}", "hej");
}

fn main() {
    let string = "{\"id\": 2,\
                  \"method\": \"hej\",\
                  \"params\": {\"sub-field\": 123}}";
    let req = serde_json::from_str::<Request>(string).unwrap();
    println!("oskar: {:?}", req);

    let to_send = json!({
        "processId": null,
        "rootUri": "file:///home/oskar/own_repos/tiny-lsp-client",
        "capabilities": {
            "textDocument": {
                "definition": {
                    "dynamicRegistration": true,
                    "linkSupport": true
                }
            }
        }
    });

    let req = Request {
        id: 123,
        method: "initialize".to_string(),
        params: to_send
    };

    let json = serde_json::to_string(&req).unwrap();

    let full = format!("Content-Length: {}\r\n\r\n{}", json.len(), &json);

    println!("oskar: {:?}", full);

    let mut child = Command::new("rust-analyzer")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn().unwrap();

    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();

    let res = stdin.write(full.as_bytes());
    println!("oskar stdin: {:?}", res);

    let mut res = Vec::new();
    stdout.read_to_end(&mut res);
    println!("oskar stdout: {}", String::from_utf8(res).unwrap());
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Request {
    pub id: u32,
    pub method: String,
    pub params: serde_json::Value,
}
