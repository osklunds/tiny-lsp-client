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
mod connection;

fn main() {

}

