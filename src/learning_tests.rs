#![allow(warnings)]

use serde::Deserialize;
use serde::Serialize;
use serde_json::{json, Number, Value};
use std::io::BufRead;
use std::io::Read;
use std::io::Write;
use std::process::{Command, Stdio};
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;
use std::time::Duration;

#[test]
fn rust_analyzer() {
    let mut child = Command::new("rust-analyzer")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let mut stderr = child.stderr.take().unwrap();

    let initialize_params = json!({
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

    let initialize_request = Request {
        id: 123,
        method: "initialize".to_string(),
        params: initialize_params,
    };

    send_request(&initialize_request, &mut stdin);

    let (stdout_tx, stdout_rx) = mpsc::channel();

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

                let size = header_value.parse::<usize>().unwrap();
                // println!("oskar: {:?}", size);

                let mut json_buf = Vec::new();
                // todo: +2 might be related the the pops above. Anyway,
                // need to find out why
                json_buf.resize(size + 2, 0);
                reader.read_exact(&mut json_buf);
                // println!("oskar: {:?}", json_buf);
                let json = String::from_utf8(json_buf).unwrap();
                // println!("oskar: {}", json);

                let parsed: Value = serde_json::from_str(&json).unwrap();
                // println!("oskar: {:?}", parsed);

                stdout_tx.send(parsed).unwrap();
            }
        }
    });

    thread::spawn(move || loop {
        let mut buf = [0; 100];
        let len = stderr.read(&mut buf).unwrap();
        println!(
            "oskar stderr: {} {}",
            len,
            String::from_utf8(buf.to_vec()).unwrap()
        );
    });

    let initialize_response = stdout_rx.recv().unwrap();
    assert_eq!(
        Value::Number(Number::from_u128(123).unwrap()),
        initialize_response["id"]
    );
    let result = &initialize_response["result"];
    let capabilities = &result["capabilities"];
    assert_eq!(Value::Bool(true), capabilities["definitionProvider"]);
    // println!("oskar: {}", initialize_response);

    let initialized_notification = json!({
        "method": "initialized",
        "params":{}
    });

    send_json(
        &serde_json::to_string(&initialized_notification).unwrap(),
        &mut stdin,
    );

    // To let indexing finish
    thread::sleep(Duration::from_secs(1));

    let find_definition_params = json!({
        "textDocument": {
            "uri": "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs"
        },
        "position": {
            "line": 4,
            "character": 4
        }
    });

    let find_definition_request = Request {
        id: 133,
        method: "textDocument/definition".to_string(),
        params: find_definition_params,
    };

    send_request(&find_definition_request, &mut stdin);
    let find_definition_response = stdout_rx.recv().unwrap();
    assert_eq!(
        Value::Number(Number::from_u128(133).unwrap()),
        find_definition_response["id"]
    );

    let result_vec = &find_definition_response["result"];
    let result = &result_vec[0];
    let target_range = &result["targetSelectionRange"];
    let start = &target_range["start"];
    assert_eq!(Value::Number(Number::from_u128(7).unwrap()), start["line"]);
    assert_eq!(
        Value::Number(Number::from_u128(3).unwrap()),
        start["character"]
    );
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Request {
    pub id: u32,
    pub method: String,
    pub params: serde_json::Value,
}

fn send_json<W: Write>(json: &str, writer: &mut W) {
    let full = format!("Content-Length: {}\r\n\r\n{}", json.len(), &json);
    // println!("oskar sending: {:?}", full);
    writer.write(full.as_bytes()).unwrap();
}

fn send_request<W: Write>(request: &Request, writer: &mut W) {
    let json = serde_json::to_string(&request).unwrap();
    send_json(&json, writer);
}
