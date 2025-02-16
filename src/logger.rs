#[cfg(test)]
mod tests;

use std::ffi::CString;
use std::fs;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;
use std::ptr;
use std::str;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Mutex;

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

macro_rules! is_log_enabled {
    ($log_name:ident) => {
        crate::logger::$log_name.load(Ordering::Relaxed)
    };
}
pub(crate) use is_log_enabled;

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
    if is_log_enabled!(LOG_IO) {
        log("IO", msg);
    }
}

pub fn log_stderr_fun<S: AsRef<str>>(msg: S) {
    if is_log_enabled!(LOG_STDERR) {
        log("STDERR", msg);
    }
}

pub fn log_rust_debug_fun<S: AsRef<str>>(msg: S) {
    if is_log_enabled!(LOG_RUST_DEBUG) {
        log("RUST_DEBUG", msg);
    }
}

pub fn log_emacs_debug_fun<S: AsRef<str>>(msg: S) {
    if is_log_enabled!(LOG_RUST_DEBUG) {
        log("EMACS_DEBUG", msg);
    }
}

fn log<L: AsRef<str>, M: AsRef<str>>(log_name: L, msg: M) {
    let timestamp = get_timestamp();
    let formatted =
        format!("{} - {} - {}\n", log_name.as_ref(), timestamp, msg.as_ref());

    let truncated = if formatted.len() > MAX_LOG_ENTRY_LEN_BYTES {
        format!("{}...TRUNCATED\n", &formatted[0..MAX_LOG_ENTRY_LEN_BYTES])
    } else {
        formatted
    };

    let mut locked_log_file_info = LOG_FILE_INFO.lock().unwrap();
    let log_file_info = locked_log_file_info.as_mut().unwrap();

    // No file because nothing has been logged yet or because nothing
    // has been logged since last time it was changed
    if log_file_info.file.is_none() {
        let new_log_file_name = &log_file_info.log_file_name;

        rotate_to_old_file(new_log_file_name);

        let file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(new_log_file_name)
            .unwrap();
        log_file_info.file = Some(file);
    };

    let log_file = log_file_info.file.as_mut().unwrap();
    write!(log_file, "{}", truncated).unwrap();

    if log_file.metadata().unwrap().len() > MAX_LOG_FILE_SIZE_BYTES {
        rotate_to_old_file(&log_file_info.log_file_name);
    }

    if is_log_enabled!(LOG_TO_STDIO) {
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

// Nothing against Chrono at all, but for this project I wanted to avoid non
// rust-lang dependencies except serde. But it seems e.g. rust-bindgen has non
// rust-lang dependencies anyway... Oh well :)
fn get_timestamp() -> String {
    let mut buffer = [0; 26];
    let ms;
    unsafe {
        let mut tv: libc::timeval = libc::timeval {
            tv_sec: 0,
            tv_usec: 0,
        };

        libc::gettimeofday(&mut tv as *mut libc::timeval, ptr::null_mut());
        let tm_info: *mut libc::tm =
            libc::localtime(&tv.tv_sec as *const libc::time_t);
        let format = CString::new("%Y-%m-%d %H:%M:%S").unwrap();
        libc::strftime(
            buffer.as_mut_ptr() as *mut i8,
            26,
            format.as_ptr(),
            tm_info,
        );
        ms = tv.tv_usec as f32 / 1000.0;
    }

    let len = buffer.iter().position(|&e| e == 0).unwrap();
    let utf8 = str::from_utf8(&buffer[0..len]).unwrap();
    format!("{}.{:03.0}", utf8, ms)
}
