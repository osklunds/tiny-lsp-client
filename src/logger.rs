include!(concat!(env!("OUT_DIR"), "/time_h_bindings.rs"));

#[cfg(test)]
mod tests;

use std::fs;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Mutex;
use std::ptr;
// todo: remove this dependency
use chrono::Local;

// todo: consider some rust-level automated tests for this module. At least
// mode-test covers it partially. There are some subtle aspects, such as
// no write to file until user has been able to change path, that would
// be good to test

macro_rules! log_io {
    ($($arg:tt)*) => {
        crate::logger::log_io_fun(format!($($arg)*));
    }
}
pub(crate) use log_io;

macro_rules! log_stderr {
    ($($arg:tt)*) => {
        crate::logger::log_stderr_fun(format!($($arg)*));
    }
}
pub(crate) use log_stderr;

macro_rules! log_rust_debug {
    ($($arg:tt)*) => {
        crate::logger::log_rust_debug_fun(format!($($arg)*));
    }
}
pub(crate) use log_rust_debug;

macro_rules! log_emacs_debug {
    ($($arg:tt)*) => {
        crate::logger::log_emacs_debug_fun(format!($($arg)*));
    }
}
pub(crate) use log_emacs_debug;

pub static LOG_IO: AtomicBool = AtomicBool::new(true);
pub static LOG_STDERR: AtomicBool = AtomicBool::new(true);
pub static LOG_RUST_DEBUG: AtomicBool = AtomicBool::new(true);
pub static LOG_TO_STDIO: AtomicBool = AtomicBool::new(true);
static LOG_FILE_INFO: Mutex<Option<LogFileInfo>> = Mutex::new(None);

const MAX_LOG_FILE_SIZE_BYTES: u64 = 10_000_000; // 10 MB
const MAX_LOG_ENTRY_LEN_BYTES: usize = 2000;

struct LogFileInfo {
    log_file_name: String,
    file: Option<File>,
}

pub fn get_log_file_name() -> Option<String> {
    let log_file_info = LOG_FILE_INFO.lock().unwrap();
    if let Some(log_file_info) = &*log_file_info {
        Some(log_file_info.log_file_name.clone())
    } else {
        None
    }
}

pub fn set_log_file_name<S: AsRef<str>>(new_log_file_name: S) {
    let mut locked_log_file_info = LOG_FILE_INFO.lock().unwrap();
    if let Some(ref mut log_file_info) = locked_log_file_info.as_mut() {
        if new_log_file_name.as_ref() == log_file_info.log_file_name {
            return;
        }
    }

    let new_log_file_info = LogFileInfo {
        log_file_name: new_log_file_name.as_ref().to_string(),
        file: None,
    };

    *locked_log_file_info = Some(new_log_file_info);
}

pub fn log_io_fun<S: AsRef<str>>(msg: S) {
    if LOG_IO.load(Ordering::Relaxed) {
        log("IO", msg);
    }
}

pub fn log_stderr_fun<S: AsRef<str>>(msg: S) {
    if LOG_STDERR.load(Ordering::Relaxed) {
        log("STDERR", msg);
    }
}

pub fn log_rust_debug_fun<S: AsRef<str>>(msg: S) {
    if LOG_RUST_DEBUG.load(Ordering::Relaxed) {
        log("RUST_DEBUG", msg);
    }
}

pub fn log_emacs_debug_fun<S: AsRef<str>>(msg: S) {
    if LOG_RUST_DEBUG.load(Ordering::Relaxed) {
        log("EMACS_DEBUG", msg);
    }
}

pub fn log_rust_debug_enabled() -> bool {
    LOG_RUST_DEBUG.load(Ordering::Relaxed)
}

fn log<L: AsRef<str>, M: AsRef<str>>(log_name: L, msg: M) {
    let timestamp = Local::now().format("%Y-%m-%d %H:%M:%S%.3f");
    let formatted =
        format!("{} - {} - {}\n", log_name.as_ref(), timestamp, msg.as_ref());

    let truncated = if formatted.len() > MAX_LOG_ENTRY_LEN_BYTES {
        format!("{}...TRUNCATED\n", &formatted[0..MAX_LOG_ENTRY_LEN_BYTES])
    } else {
        formatted
    };

    let mut locked_log_file_info = LOG_FILE_INFO.lock().unwrap();
    let mut log_file_info = locked_log_file_info.as_mut().unwrap();

    // No file because nothing has been logged yet or because nothing
    // has been logged since last time it was changed
    if log_file_info.file.is_none() {
        let new_log_file_name = &log_file_info.log_file_name;

        rotate_to_old_file(new_log_file_name);

        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(new_log_file_name)
            .unwrap();
        log_file_info.file = Some(file);
    };

    let mut log_file = log_file_info.file.as_mut().unwrap();
    write!(log_file, "{}", truncated);

    if log_file.metadata().unwrap().len() > MAX_LOG_FILE_SIZE_BYTES {
        rotate_to_old_file(&log_file_info.log_file_name);
    }

    if LOG_TO_STDIO.load(Ordering::Relaxed) {
        print!("{}", truncated);
    }
}

fn rotate_to_old_file(log_file_name: &str) {
    // Can fail if new_log_file_name doesn't exist. So don't unwrap
    // and only write existing content if the new file already has content
    if let Ok(existing_content) = fs::read_to_string(log_file_name) {
        fs::write(format!("{}.old", log_file_name), existing_content).unwrap();
    }
    fs::write(log_file_name, "").unwrap();
}

fn get_timestamp() -> String {
    unsafe {
        let timer: time_t = time(ptr::null_mut());
    }

    // strftime();
    "hej".to_string()
}
