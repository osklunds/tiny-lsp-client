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

pub unsafe fn extract_integer(
    env: *mut emacs_env,
    integer: emacs_value,
) -> i64 {
    handle_non_local_exit(env, || (*env).extract_integer.unwrap()(env, integer))
}

pub unsafe fn extract_bool(env: *mut emacs_env, value: emacs_value) -> bool {
    extract_string(env, call(env, "symbol-name", vec![value])) != "nil"
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

pub unsafe fn call1<F: AsRef<str>, T: IntoLisp>(
    env: *mut emacs_env,
    function_name: F,
    arg1: T,
) -> Option<emacs_value> {
    if let Some(arg1) = arg1.into_lisp(env) {
        call_new(env, function_name, vec![arg1])
    } else {
        None
    }
}

// todo: consider call1, call2 etc to avoid vec as argument
pub unsafe fn call_new_from_lisp<F: AsRef<str>, T: FromLisp>(
    env: *mut emacs_env,
    func: F,
    args: Vec<emacs_value>,
) -> Option<T> {
    if let Some(ret) = call_new(env, func, args) {
        T::from_lisp(env, ret)
    } else {
        None
    }
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
    // todo: don't unwrap
    call(env, "nth", vec![index.into_lisp(env).unwrap(), list])
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

pub unsafe fn handle_none<T: IntoLisp, F: FnMut() -> T>(
    env: *mut emacs_env,
    mut func: F,
) -> emacs_value {
    let value = func();
    if let Some(value) = value.into_lisp(env) {
        value
    } else {
        todo!()
    }
}

unsafe fn handle_non_local_exit_new<F: FnMut() -> R, R>(
    env: *mut emacs_env,
    mut func: F,
) -> Option<R> {
    let result = func();
    let status = (*env).non_local_exit_check.unwrap()(env);
    if status == emacs_funcall_exit_emacs_funcall_exit_return {
        Some(result)
    } else {
        // logger can only be called once the log file has been initialized,
        // which is not the case in emacs_module_init for example. But for
        // now, gamble that no non-local exists until log file has been
        // created.
        // logger::log_rust_debug!("non local exit: {}", status);
        None
    }
}

pub unsafe fn intern_new(
    env: *mut emacs_env,
    symbol: &str,
) -> Option<emacs_value> {
    let symbol = CString::new(symbol).unwrap();
    handle_non_local_exit_new(env, || {
        (*env).intern.unwrap()(env, symbol.as_ptr())
    })
}

pub unsafe fn call_new<F: AsRef<str>>(
    env: *mut emacs_env,
    func: F,
    mut args: Vec<emacs_value>,
) -> Option<emacs_value> {
    let funcall = (*env).funcall.unwrap();
    handle_non_local_exit_new(env, || {
        funcall(
            env,
            intern(env, func.as_ref()),
            args.len() as isize,
            args.as_mut_ptr(),
        )
    })
}

// @credits: IntoLisp/FromLisp inspired by
// https://github.com/ubolonton/emacs-module-rs
pub trait IntoLisp {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value>;
}

pub struct Symbol(pub String);

pub fn symbol<S: ToString>(s: S) -> Symbol {
    Symbol(s.to_string())
}

impl IntoLisp for Symbol {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        intern_new(env, &self.0)
    }
}

// todo: unify IntoLisp for strings
impl IntoLisp for String {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let make_string = (*env).make_string.unwrap();
        let c_string = CString::new(self.as_str()).unwrap();
        let len = c_string.as_bytes().len() as isize;
        let c_string_ptr = c_string.as_ptr();
        handle_non_local_exit_new(env, || make_string(env, c_string_ptr, len))
    }
}

impl IntoLisp for &String {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let make_string = (*env).make_string.unwrap();
        let c_string = CString::new(self.as_str()).unwrap();
        let len = c_string.as_bytes().len() as isize;
        let c_string_ptr = c_string.as_ptr();
        handle_non_local_exit_new(env, || make_string(env, c_string_ptr, len))
    }
}

impl IntoLisp for &str {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let make_string = (*env).make_string.unwrap();
        let c_string = CString::new(self).unwrap();
        let len = c_string.as_bytes().len() as isize;
        let c_string_ptr = c_string.as_ptr();
        handle_non_local_exit_new(env, || make_string(env, c_string_ptr, len))
    }
}

impl IntoLisp for usize {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        handle_non_local_exit_new(env, || {
            (*env).make_integer.unwrap()(env, self as i64)
        })
    }
}

impl IntoLisp for i32 {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        handle_non_local_exit_new(env, || {
            (*env).make_integer.unwrap()(env, self as i64)
        })
    }
}

impl IntoLisp for u32 {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        handle_non_local_exit_new(env, || {
            (*env).make_integer.unwrap()(env, self as i64)
        })
    }
}

impl IntoLisp for i64 {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        handle_non_local_exit_new(env, || {
            (*env).make_integer.unwrap()(env, self)
        })
    }
}

impl IntoLisp for bool {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        if self {
            intern_new(env, "t")
        } else {
            intern_new(env, "nil")
        }
    }
}

impl<T: IntoLisp> IntoLisp for Vec<T> {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let len = self.len();
        let vec_of_lisp_objects: Vec<_> = self
            .into_iter()
            .map_while(|element| element.into_lisp(env))
            .collect();

        if vec_of_lisp_objects.len() == len {
            call_new(env, "list", vec_of_lisp_objects)
        } else {
            None
        }
    }
}

impl<A: IntoLisp, B: IntoLisp, C: IntoLisp> IntoLisp for (A, B, C) {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let (a, b, c) = self;

        if let Some(a) = a.into_lisp(env) {
            if let Some(b) = b.into_lisp(env) {
                if let Some(c) = c.into_lisp(env) {
                    return call_new(env, "list", vec![a, b, c]);
                }
            }
        }
        None
    }
}

impl<A: IntoLisp, B: IntoLisp, C: IntoLisp, D: IntoLisp> IntoLisp
    for (A, B, C, D)
{
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let (a, b, c, d) = self;

        if let Some(a) = a.into_lisp(env) {
            if let Some(b) = b.into_lisp(env) {
                if let Some(c) = c.into_lisp(env) {
                    if let Some(d) = d.into_lisp(env) {
                        return call_new(env, "list", vec![a, b, c, d]);
                    }
                }
            }
        }
        None
    }
}

pub trait FromLisp {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<Self>
    where
        Self: Sized;
}

impl FromLisp for String {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        val: emacs_value,
    ) -> Option<String> {
        let copy_string_contents = (*env).copy_string_contents.unwrap();

        // First find the length
        let mut len: isize = 0;
        // todo: use ?
        if let Some(result_find_length) = handle_non_local_exit_new(env, || {
            copy_string_contents(env, val, ptr::null_mut::<i8>(), &mut len)
        }) {
            assert!(result_find_length);
            assert!(len > 0);

            // Then get the actual string
            let mut buf = vec![0; len as usize];
            if let Some(result_get_string) =
                handle_non_local_exit_new(env, || {
                    copy_string_contents(
                        env,
                        val,
                        buf.as_mut_ptr() as *mut i8,
                        &mut len,
                    )
                })
            {
                assert!(result_get_string);
                len -= 1; // remove null-terminator
                Some(str::from_utf8(&buf[0..len as usize]).unwrap().to_string())
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl FromLisp for bool {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<bool> {
        if let Some(string) =
            call_new_from_lisp::<&str, String>(env, "symbol-name", vec![value])
        {
            Some(string != "nil")
        } else {
            None
        }
    }
}

// todo: macro for all integers
impl FromLisp for i64 {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<i64> {
        handle_non_local_exit_new(env, || {
            (*env).extract_integer.unwrap()(env, value)
        })
    }
}

impl FromLisp for usize {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<usize> {
        handle_non_local_exit_new(env, || {
            (*env).extract_integer.unwrap()(env, value) as usize
        })
    }
}

impl<A: FromLisp, B: FromLisp> FromLisp for (A, B) {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<(A, B)> {
        // todo: don't unwrap
        // todo: throw lisp exception instead of crashing
        assert!(call_new_from_lisp::<&str, bool>(env, "listp", vec![value])
            .unwrap());
        assert_eq!(
            call_new_from_lisp::<&str, i64>(env, "length", vec![value])
                .unwrap(),
            2
        );

        if let Some(a) = call_new_from_lisp(
            env,
            "nth",
            vec![0.into_lisp(env).unwrap(), value],
        ) {
            if let Some(b) = call_new_from_lisp(
                env,
                "nth",
                vec![1.into_lisp(env).unwrap(), value],
            ) {
                return Some((a, b));
            }
        }

        None
    }
}

impl<A: FromLisp, B: FromLisp, C: FromLisp> FromLisp for (A, B, C) {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<(A, B, C)> {
        // todo: don't unwrap
        // todo: throw lisp exception instead of crashing
        assert!(call_new_from_lisp::<&str, bool>(env, "listp", vec![value])
            .unwrap());
        assert_eq!(
            call_new_from_lisp::<&str, i64>(env, "length", vec![value])
                .unwrap(),
            3
        );

        if let Some(a) = call_new_from_lisp(
            env,
            "nth",
            vec![0.into_lisp(env).unwrap(), value],
        ) {
            if let Some(b) = call_new_from_lisp(
                env,
                "nth",
                vec![1.into_lisp(env).unwrap(), value],
            ) {
                if let Some(c) = call_new_from_lisp(
                    env,
                    "nth",
                    vec![2.into_lisp(env).unwrap(), value],
                ) {
                    return Some((a, b, c));
                }
            }
        }

        None
    }
}

impl<A: FromLisp, B: FromLisp, C: FromLisp, D: FromLisp, E: FromLisp> FromLisp
    for (A, B, C, D, E)
{
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<(A, B, C, D, E)> {
        // todo: don't unwrap
        // todo: throw lisp exception instead of crashing
        assert!(call_new_from_lisp::<&str, bool>(env, "listp", vec![value])
            .unwrap());
        assert_eq!(
            call_new_from_lisp::<&str, i64>(env, "length", vec![value])
                .unwrap(),
            5
        );

        if let Some(a) = call_new_from_lisp(
            env,
            "nth",
            vec![0.into_lisp(env).unwrap(), value],
        ) {
            if let Some(b) = call_new_from_lisp(
                env,
                "nth",
                vec![1.into_lisp(env).unwrap(), value],
            ) {
                if let Some(c) = call_new_from_lisp(
                    env,
                    "nth",
                    vec![2.into_lisp(env).unwrap(), value],
                ) {
                    if let Some(d) = call_new_from_lisp(
                        env,
                        "nth",
                        vec![3.into_lisp(env).unwrap(), value],
                    ) {
                        if let Some(e) = call_new_from_lisp(
                            env,
                            "nth",
                            vec![4.into_lisp(env).unwrap(), value],
                        ) {
                            return Some((a, b, c, d, e));
                        }
                    }
                }
            }
        }

        None
    }
}

impl<T: FromLisp> FromLisp for Vec<T> {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<Vec<T>> {
        // todo: don't unwrap
        // todo: throw lisp exception instead of crashing
        assert!(call_new_from_lisp::<&str, bool>(env, "listp", vec![value])
            .unwrap());
        let len = call_new_from_lisp::<&str, i64>(env, "length", vec![value])
            .unwrap();

        let mut vec = Vec::new();

        for i in 0..len {
            if let Some(i) = i.into_lisp(env) {
                if let Some(element) =
                    call_new_from_lisp(env, "nth", vec![i, value])
                {
                    vec.push(element);
                    continue;
                }
            }
            return None;
        }

        Some(vec)
    }
}

impl<T: FromLisp> FromLisp for Option<T> {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<Option<T>> {
        if let Some(is_nil) = call_new_from_lisp(env, "null", vec![value]) {
            if is_nil {
                Some(None)
            } else {
                T::from_lisp(env, value).map(|v| Some(v))
            }
        } else {
            None
        }
    }
}
