// Copyright (C) 2025 Oskar Lundström

// This file is part of tiny-lsp-client.

// tiny-lsp-client is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.

// tiny-lsp-client is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.

// You should have received a copy of the GNU General Public License along with
// tiny-lsp-client. If not, see <https://www.gnu.org/licenses/>.

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
// todo: more blocklist to fix this
#![allow(dead_code)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use crate::logger;
use std::ffi::CString;
use std::ptr;
use std::str;

pub unsafe fn extract_string(env: *mut emacs_env, val: emacs_value) -> String {
    let copy_string_contents = (*env).copy_string_contents.unwrap();

    // First find the length
    let mut len: isize = 0;
    let result_find_length = handle_non_local_exit(env, || {
        copy_string_contents(env, val, ptr::null_mut::<i8>(), &mut len)
    });
    assert!(result_find_length);
    assert!(len > 0);

    // Then get the actual string
    let mut buf = vec![0; len as usize];
    let result_get_string = handle_non_local_exit(env, || {
        copy_string_contents(env, val, buf.as_mut_ptr() as *mut i8, &mut len)
    });

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
    handle_non_local_exit(env, || make_string(env, c_string.as_ptr(), len))
}

pub unsafe fn extract_integer(
    env: *mut emacs_env,
    integer: emacs_value,
) -> i64 {
    handle_non_local_exit(env, || (*env).extract_integer.unwrap()(env, integer))
}

pub unsafe fn make_integer(env: *mut emacs_env, integer: i64) -> emacs_value {
    handle_non_local_exit(env, || (*env).make_integer.unwrap()(env, integer))
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
    symbol: &str,
) {
    let make_function = (*env).make_function.unwrap();

    let emacs_fun = handle_non_local_exit(env, || {
        make_function(
            env,
            min_arity,
            max_arity,
            Some(fun),
            // No docstring, but it's just internal functions and all have
            // relatively self-explanatory names
            ptr::null_mut(),
            ptr::null_mut(),
        )
    });
    call(env, "fset", vec![intern(env, symbol), emacs_fun]);
}

pub unsafe fn call<F: AsRef<str>>(
    env: *mut emacs_env,
    func: F,
    mut args: Vec<emacs_value>,
) -> emacs_value {
    let funcall = (*env).funcall.unwrap();
    handle_non_local_exit(env, || {
        funcall(
            env,
            intern(env, func.as_ref()),
            args.len() as isize,
            args.as_mut_ptr(),
        )
    })
}

pub unsafe fn intern(env: *mut emacs_env, symbol: &str) -> emacs_value {
    let symbol = CString::new(symbol).unwrap();
    handle_non_local_exit(env, || (*env).intern.unwrap()(env, symbol.as_ptr()))
}

pub unsafe fn nth(
    env: *mut emacs_env,
    index: i64,
    list: emacs_value,
) -> emacs_value {
    call(env, "nth", vec![make_integer(env, index), list])
}

unsafe fn handle_non_local_exit<F: FnMut() -> R, R>(
    env: *mut emacs_env,
    mut func: F,
) -> R {
    // Consider better way of handling non local exit than trying again
    loop {
        let result = func();
        let status = (*env).non_local_exit_check.unwrap()(env);
        if status == emacs_funcall_exit_emacs_funcall_exit_return {
            return result;
        } else {
            // logger can only be called once the log file has been initialized,
            // which is not the case in emacs_module_init for example. But for
            // now, gamble that no non-local exists until log file has been
            // created.
            logger::log_rust_debug!("non local exit: {}", status);
            (*env).non_local_exit_clear.unwrap()(env);
        }
    }
}
