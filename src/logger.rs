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

macro_rules! log_stderr {
    ($($arg:tt)*) => {
        crate::logger::log_stderr_fun(format!($($arg)*));
    }
}
pub(crate) use log_stderr;

macro_rules! log_debug {
    ($($arg:tt)*) => {
        crate::logger::log_debug_fun(format!($($arg)*));
    }
}
pub(crate) use log_debug;

macro_rules! log_emacs_debug {
    ($($arg:tt)*) => {
        crate::logger::log_emacs_debug_fun(format!($($arg)*));
    }
}
pub(crate) use log_emacs_debug;

pub static LOG_IO: AtomicBool = AtomicBool::new(true);
pub static LOG_STDERR: AtomicBool = AtomicBool::new(true);
pub static LOG_DEBUG: AtomicBool = AtomicBool::new(true);
pub static LOG_TO_STDIO: AtomicBool = AtomicBool::new(true);
static LOG_FILE_INFO: Mutex<Option<LogFileInfo>> = Mutex::new(None);

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

pub fn log_debug_fun<S: AsRef<str>>(msg: S) {
    if LOG_DEBUG.load(Ordering::Relaxed) {
        log("DEBUG", msg);
    }
}

pub fn log_emacs_debug_fun<S: AsRef<str>>(msg: S) {
    if LOG_DEBUG.load(Ordering::Relaxed) {
        log("EMACS_DEBUG", msg);
    }
}

pub fn log_debug_enabled() -> bool {
    LOG_DEBUG.load(Ordering::Relaxed)
}

fn log<L: AsRef<str>, M: AsRef<str>>(log_name: L, msg: M) {
    let timestamp = Local::now().format("%Y-%m-%d %H:%M:%S%.3f");
    // todo: include root path
    let formatted =
        format!("{} - {} - {}\n", log_name.as_ref(), timestamp, msg.as_ref());

    let mut locked_log_file_info = LOG_FILE_INFO.lock().unwrap();
    let mut log_file_info = locked_log_file_info.as_mut().unwrap();
    if let Some(mut log_file) = log_file_info.file.as_mut() {
        write!(log_file, "{}", formatted);
    } else {
        let new_log_file_name = &log_file_info.log_file_name;

        // Can fail if new_log_file_name doesn't exist
        if let Ok(old_content) = fs::read_to_string(new_log_file_name) {
            fs::write(format!("{}.old", new_log_file_name), old_content)
                .unwrap();
        }

        fs::write(new_log_file_name, "").unwrap();
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(new_log_file_name)
            .unwrap();
        write!(file, "{}", formatted);
        log_file_info.file = Some(file);
    }

    if LOG_TO_STDIO.load(Ordering::Relaxed) {
        print!("{}", formatted);
    }
}
