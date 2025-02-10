use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::path::Path;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Mutex;
// todo: remove this dependency
use chrono::Local;

macro_rules! log_io {
    ($($arg:tt)*) => {
        crate::logger::log_io_fun(format!($($arg)*));
    }
}
pub(crate) use log_io;

macro_rules! log_debug {
    ($($arg:tt)*) => {
        crate::logger::log_debug_fun(format!($($arg)*));
    }
}
pub(crate) use log_debug;

pub static LOG_TO_STDIO: AtomicBool = AtomicBool::new(true);
static LOG_FILE_NAME: Mutex<String> = Mutex::new(String::new());
static LOGGER: Mutex<Option<File>> = Mutex::new(None);

pub fn set_log_file_name(log_file_name: String) {
    let mut static_log_file_name = LOG_FILE_NAME.lock().unwrap();
    if log_file_name != *static_log_file_name {
        let mut logger = LOGGER.lock().unwrap();
        let mut file = OpenOptions::new()
            .append(true)
            .open(&log_file_name)
            .unwrap();
        *logger = Some(file);
        *static_log_file_name = log_file_name;
    }
}

pub fn log_io_fun<S: AsRef<str>>(msg: S) {
    let timestamp = Local::now().format("%Y-%m-%d %H:%M:%S%.3f");

    if LOG_TO_STDIO.load(Ordering::Relaxed) {
        print!("IO {} - {}\n", timestamp, msg.as_ref());
    }
}

pub fn log_debug_fun<S: AsRef<str>>(msg: S) {
    let timestamp = Local::now().format("%Y-%m-%d %H:%M:%S%.3f");

    if LOG_TO_STDIO.load(Ordering::Relaxed) {
        print!("DEBUG {} - {}\n", timestamp, msg.as_ref());
    }
}
