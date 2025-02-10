use std::fs;
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

static LOG_IO: AtomicBool = AtomicBool::new(true);
static LOG_STDERR: AtomicBool = AtomicBool::new(true);
static DEBUG_LOGS: AtomicBool = AtomicBool::new(true);
static LOG_TO_STDIO: AtomicBool = AtomicBool::new(true);
static LOG_FILE: Mutex<Option<(String, File)>> = Mutex::new(None);

pub fn set_log_file_name<S: AsRef<str>>(new_log_file_name: S) {
    let mut binding = LOG_FILE.lock().unwrap();
    if let Some((ref mut log_file_name, ref mut log_file)) = binding.as_mut() {
        if new_log_file_name.as_ref() == *log_file_name {
            return;
        }
    }
    fs::write(new_log_file_name.as_ref(), "").unwrap();
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(new_log_file_name.as_ref())
        .unwrap();
    *binding = Some((new_log_file_name.as_ref().to_string(), file));
}

pub fn log_io_fun<S: AsRef<str>>(msg: S) {
    log("IO   ", msg);
}

pub fn log_debug_fun<S: AsRef<str>>(msg: S) {
    log("DEBUG", msg);
}

fn log<L: AsRef<str>, M: AsRef<str>>(log_name: L, msg: M) {
    let timestamp = Local::now().format("%Y-%m-%d %H:%M:%S%.3f");
    let formatted =
        format!("{} - {} - {}", log_name.as_ref(), timestamp, msg.as_ref());

    let mut binding = LOG_FILE.lock().unwrap();
    let (_, log_file) = binding.as_mut().unwrap();
    write!(log_file, "{}", formatted);

    if LOG_TO_STDIO.load(Ordering::Relaxed) {
        print!("{}", formatted);
    }
}
