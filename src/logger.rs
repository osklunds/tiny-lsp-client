
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use chrono::Local;

macro_rules! log_io {
    ($($arg:tt)*) => {
        crate::logger::log_io_fun(format!($($arg)*));
    }
}
pub(crate) use log_io;

pub static LOG_TO_STDIO: AtomicBool = AtomicBool::new(true);

pub fn log_io_fun<S: AsRef<str>>(msg: S) {
    let timestamp = Local::now().format("%Y-%m-%d %H:%M:%S%.3f");

    let formatted_msg = format!("{} - {}\n", timestamp, msg.as_ref());

    if LOG_TO_STDIO.load(Ordering::Relaxed) {
        print!("{}", formatted_msg);
    }
}
