
#![allow(warnings)] 

use std::process::{Command, Stdio};
use std::io::Read;
use std::io::Write;
use serde_json::{json, Value};
use serde::Deserialize;
use serde::Serialize;
use std::thread;
use std::time::Duration;
use std::io::BufRead;

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

    let mut child = Command::new("rust-analyzer")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn().unwrap();

    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();

    let res = send_request(&req, &mut stdin);
    println!("oskar stdin: {:?}", res);

    thread::spawn(move || {
        let mut reader = std::io::BufReader::new(stdout);

        loop {
            let mut buf = String::new();
            loop {
                buf.clear();
                reader.read_line(&mut buf).unwrap();
                buf.pop();
                buf.pop();
                let parts: Vec<&str> = buf.split(": ").collect();
                let header_name = parts[0];
                let header_value = parts[1];

                // println!("oskar: {:?}", header_name);

                let size = header_value.parse::<usize>().unwrap();
                println!("oskar: {:?}", size);

                let mut json_buf = Vec::new();
                // todo: +2 might be related the the pops above. Anyway,
                // need to find out why
                json_buf.resize(size+2, 0);
                reader.read_exact(&mut json_buf);
                // println!("oskar: {:?}", json_buf);
                let json = String::from_utf8(json_buf).unwrap();
                // println!("oskar: {}", json);

                let parsed: Value = serde_json::from_str(&json).unwrap();
                println!("oskar: {:?}", parsed);
            }
        }
    });

    thread::spawn(move || {
        loop {
            let mut buf = [0; 100];
            let len = stderr.read(&mut buf).unwrap();
            println!("oskar stderr: {} {}", len, String::from_utf8(buf.to_vec()).unwrap());

            if len == 0 {
                break;
            }
        }
    });

    thread::sleep(Duration::from_secs(1));



    thread::sleep(Duration::from_secs(60));
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Request {
    pub id: u32,
    pub method: String,
    pub params: serde_json::Value,
}

fn send_request<W: Write>(request: &Request, writer: &mut W) {
    let json = serde_json::to_string(&request).unwrap();
    let full = format!("Content-Length: {}\r\n\r\n{}", json.len(), &json);
    println!("oskar sending: {:?}", full);
    writer.write(full.as_bytes()).unwrap();
}

    
