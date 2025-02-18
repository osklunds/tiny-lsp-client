#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
// todo: more blocklist to fix this
#![allow(dead_code)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use std::ffi::CString;
use std::ptr;
use std::str;

pub unsafe fn extract_string(env: *mut emacs_env, val: emacs_value) -> String {
    let copy_string_contents = (*env).copy_string_contents.unwrap();

    // First find the length
    let mut len = 0;
    let result_find_length =
        copy_string_contents(env, val, ptr::null_mut::<i8>(), &mut len);
    assert!(result_find_length);
    assert!(len > 0);

    // Then get the actual string
    let mut buf = vec![0; len as usize];
    let result_get_string =
        copy_string_contents(env, val, buf.as_mut_ptr() as *mut i8, &mut len);

    assert!(result_get_string);
    len -= 1; // remove null-terminator
    str::from_utf8(&buf[0..len as usize]).unwrap().to_string()
}

pub unsafe fn make_string<S: AsRef<str>>(
    env: *mut emacs_env,
    string: S,
) -> emacs_value {
    let make_string = (*env).make_string.unwrap();
    let c_string = CString::new(string.as_ref()).unwrap();
    let len = c_string.as_bytes().len() as isize;
    make_string(env, c_string.as_ptr(), len)
}

pub unsafe fn extract_integer(
    env: *mut emacs_env,
    integer: emacs_value,
) -> i64 {
    (*env).extract_integer.unwrap()(env, integer)
}

pub unsafe fn make_integer(env: *mut emacs_env, integer: i64) -> emacs_value {
    (*env).make_integer.unwrap()(env, integer)
}

pub unsafe fn extract_bool(env: *mut emacs_env, value: emacs_value) -> bool {
    extract_string(env, call(env, "symbol-name", vec![value])) != "nil"
}

pub unsafe fn make_bool(env: *mut emacs_env, value: bool) -> emacs_value {
    if value {
        intern(env, "t")
    } else {
        intern(env, "nil")
    }
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

    let docstring = CString::new(docstring).unwrap();
    let emacs_fun = make_function(
        env,
        min_arity,
        max_arity,
        Some(fun),
        docstring.as_ptr(),
        ptr::null_mut(),
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
    let symbol = CString::new(symbol).unwrap();
    (*env).intern.unwrap()(env, symbol.as_ptr())
}

pub unsafe fn nth(
    env: *mut emacs_env,
    index: i64,
    list: emacs_value,
) -> emacs_value {
    call(env, "nth", vec![make_integer(env, index), list])
}
