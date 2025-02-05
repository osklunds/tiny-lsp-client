
include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use std::ffi::CString;

macro_rules! c_string {
    ($x:expr) => {
        CString::new($x).unwrap().as_ptr()
    };
}

pub unsafe fn extract_string(env: *mut emacs_env, val: emacs_value) -> String {
    let copy_string_contents = (*env).copy_string_contents.unwrap();
    let mut buf = vec![0; 1000];
    let mut len = 1000;
    let res =
        copy_string_contents(env, val, buf.as_mut_ptr() as *mut i8, &mut len);
    assert!(res);
    len -= 1;
    std::str::from_utf8(&buf[0..len as usize])
        .unwrap()
        .to_string()
}

pub unsafe fn make_string(env: *mut emacs_env, string: String) -> emacs_value {
    let make_string = (*env).make_string.unwrap();
    let c_string = CString::new(string).unwrap();
    let len = c_string.as_bytes().len() as isize;
    make_string(env, c_string.as_ptr(), len)
}

pub unsafe fn extract_integer(env: *mut emacs_env, integer: emacs_value) -> i64 {
    (*env).extract_integer.unwrap()(env, integer)
}

pub unsafe fn make_integer(env: *mut emacs_env, integer: i64) -> emacs_value {
    (*env).make_integer.unwrap()(env, integer)
}

pub unsafe fn export_function(
    env: *mut emacs_env,
    min_arity: isize,
    max_arity: isize,
    fun: unsafe extern "C" fn(
        env: *mut emacs_env,
        nargs: isize,
        args: *mut emacs_value,
        data: *mut ::std::os::raw::c_void,
    ) -> emacs_value,
    docstring: &str,
    symbol: &str,
) {
    let make_function = (*env).make_function.unwrap();

    let emacs_fun = make_function(
        env,
        min_arity,
        max_arity,
        Some(fun),
        c_string!(docstring),
        std::ptr::null_mut(),
    );
    call(env, "fset", vec![intern(env, symbol), emacs_fun]);
}

pub unsafe fn call<F: AsRef<str>>(
    env: *mut emacs_env,
    func: F,
    mut args: Vec<emacs_value>,
) -> emacs_value {
    let funcall = (*env).funcall.unwrap();
    funcall(
        env,
        intern(env, func.as_ref()),
        args.len() as isize,
        args.as_mut_ptr(),
    )
}

pub unsafe fn intern(env: *mut emacs_env, symbol: &str) -> emacs_value {
    (*env).intern.unwrap()(env, c_string!(symbol))
}
