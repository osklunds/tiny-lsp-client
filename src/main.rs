
#![allow(warnings)] 

use std::process::{Command, Stdio};
use std::io::Read;
use std::io::Write;

fn main() {
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

