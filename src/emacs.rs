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
use std::sync::atomic::Ordering;

// Module initialization

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
) -> Option<()> {
    let make_function = (*env).make_function.unwrap();

    let emacs_fun = handle_non_local_exit_new(env, || {
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
    })?;
    let symbol = intern_new(env, symbol)?;
    call_lisp_lisp(env, "fset", vec![symbol, emacs_fun])?;
    Some(())
}

pub unsafe fn provide_tlc_rust(env: *mut emacs_env) -> Option<()> {
    call1_rust(env, "provide", symbol("tlc-rust"))
}

// Calling emacs functions

// To be used when calling with lisp arguments and need lisp return value
unsafe fn call_lisp_lisp<F: AsRef<str>>(
    env: *mut emacs_env,
    function_name: F,
    mut args: Vec<emacs_value>,
) -> Option<emacs_value> {
    let funcall = (*env).funcall.unwrap();
    let function_symbol = intern_new(env, function_name)?;
    handle_non_local_exit_new(env, || {
        funcall(env, function_symbol, args.len() as isize, args.as_mut_ptr())
    })
}

// To be used when calling with rust arguments and dont' care about return value
unsafe fn call1_rust<F: AsRef<str>, T: IntoLisp>(
    env: *mut emacs_env,
    function_name: F,
    arg1: T,
) -> Option<()> {
    let arg1 = arg1.into_lisp(env)?;
    call_lisp_lisp(env, function_name, vec![arg1])?;
    Some(())
}

// To be used when calling with lisp arguments but need rust return value
unsafe fn call_lisp_rust<F: AsRef<str>, T: FromLisp>(
    env: *mut emacs_env,
    func: F,
    args: Vec<emacs_value>,
) -> Option<T> {
    let ret = call_lisp_lisp(env, func, args)?;
    T::from_lisp(env, ret)
}

// Function API

// todo: wrapper that takes lambda, that takes some args. the lambda is
// safe the wrapper is unsafe

pub unsafe fn lisp_function_in_rust<
    S: AsRef<str>,
    A: FromVecOfLisp,
    R: IntoLisp,
    F: FnMut(A) -> R,
>(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    function_name: S,
    mut function: F,
) -> emacs_value {
    log_args(env, nargs, args, function_name);
    let args_vec = args_pointer_to_args_vec(nargs, args);
    let arg = FromVecOfLisp::from_vec_of_lisp(env, args_vec).unwrap();
    let ret = function(arg).into_lisp(env).unwrap();

    ret
}

pub unsafe fn log_args<S: AsRef<str>>(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    function_name: S,
) -> Option<()> {
    // logger::log_rust_debug! already knows whether to log or not. But check
    // anyway as an optimization so that lots of string and terms aren't
    // created unecessarily.
    // Idea: pass lambda that is lazily called. So can do a general
    // optimization without macros
    if logger::is_log_enabled!(LOG_RUST_DEBUG) {
        let args_list = args_pointer_to_args_vec(nargs, args);
        let list = call_lisp_lisp(env, "list", args_list)?;
        let format_string =
            format!("{} arguments ({}) : %S", function_name.as_ref(), nargs);
        let format_string = format_string.into_lisp(env)?;
        let formatted: String =
            call_lisp_rust(env, "format", vec![format_string, list])?;
        logger::log_rust_debug!("{}", formatted);
    }
    // todo: check return value
    Some(())
}

pub unsafe fn args_pointer_to_args_vec(
    nargs: isize,
    args: *mut emacs_value,
) -> Vec<emacs_value> {
    let mut args_list = Vec::new();
    for i in 0..nargs {
        args_list.push(*args.offset(i));
    }
    args_list
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

unsafe fn intern_new<T: AsRef<str>>(
    env: *mut emacs_env,
    symbol: T,
) -> Option<emacs_value> {
    let symbol = CString::new(symbol.as_ref()).unwrap();
    handle_non_local_exit_new(env, || {
        (*env).intern.unwrap()(env, symbol.as_ptr())
    })
}

// IntoLisp

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

impl FromLisp for Symbol {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<Symbol> {
        let string = call_lisp_rust(env, "symbol-name", vec![value])?;
        Some(Symbol(string))
    }
}

impl IntoLisp for String {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        AsRef::<str>::as_ref(&self).into_lisp(env)
    }
}

impl IntoLisp for &String {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        AsRef::<str>::as_ref(&self).into_lisp(env)
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

macro_rules! impl_into_lisp_for_integer {
    ($t:ident) => {
        impl IntoLisp for $t {
            unsafe fn into_lisp(
                self,
                env: *mut emacs_env,
            ) -> Option<emacs_value> {
                handle_non_local_exit_new(env, || {
                    (*env).make_integer.unwrap()(env, self as i64)
                })
            }
        }
    };
}

impl_into_lisp_for_integer!(i32);
impl_into_lisp_for_integer!(u32);
impl_into_lisp_for_integer!(i64);
impl_into_lisp_for_integer!(usize);

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
            call_lisp_lisp(env, "list", vec_of_lisp_objects)
        } else {
            None
        }
    }
}

impl<A: IntoLisp, B: IntoLisp> IntoLisp for (A, B) {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let (a, b) = self;
        let a = a.into_lisp(env)?;
        let b = b.into_lisp(env)?;
        call_lisp_lisp(env, "list", vec![a, b])
    }
}

impl<A: IntoLisp, B: IntoLisp, C: IntoLisp> IntoLisp for (A, B, C) {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let (a, b, c) = self;
        let a = a.into_lisp(env)?;
        let b = b.into_lisp(env)?;
        let c = c.into_lisp(env)?;
        call_lisp_lisp(env, "list", vec![a, b, c])
    }
}

impl<A: IntoLisp, B: IntoLisp, C: IntoLisp, D: IntoLisp> IntoLisp
    for (A, B, C, D)
{
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        let (a, b, c, d) = self;
        let a = a.into_lisp(env)?;
        let b = b.into_lisp(env)?;
        let c = c.into_lisp(env)?;
        let d = d.into_lisp(env)?;
        call_lisp_lisp(env, "list", vec![a, b, c, d])
    }
}

// FromLisp

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
        if !handle_non_local_exit_new(env, || {
            copy_string_contents(env, val, ptr::null_mut::<i8>(), &mut len)
        })? {
            return None;
        }
        if len <= 0 {
            return None;
        }

        // Then get the actual string
        let mut buf = vec![0; len as usize];
        if !handle_non_local_exit_new(env, || {
            copy_string_contents(
                env,
                val,
                buf.as_mut_ptr() as *mut i8,
                &mut len,
            )
        })? {
            return None;
        }

        len -= 1; // remove null-terminator
        Some(str::from_utf8(&buf[0..len as usize]).unwrap().to_string())
    }
}

impl FromLisp for bool {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<bool> {
        let null: Symbol = call_lisp_rust(env, "null", vec![value])?;
        Some(null.0 != "t")
    }
}

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

macro_rules! impl_from_lisp_for_integer {
    ($t:ident) => {
        impl FromLisp for $t {
            unsafe fn from_lisp(
                env: *mut emacs_env,
                value: emacs_value,
            ) -> Option<Self> {
                handle_non_local_exit_new(env, || {
                    (*env).extract_integer.unwrap()(env, value) as Self
                })
            }
        }
    };
}

impl_from_lisp_for_integer!(u64);
impl_from_lisp_for_integer!(usize);

impl<A: FromLisp, B: FromLisp> FromLisp for (A, B) {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<(A, B)> {
        if !call_lisp_rust::<&str, bool>(env, "listp", vec![value])? {
            return None;
        }

        if call_lisp_rust::<&str, i64>(env, "length", vec![value])? != 2 {
            return None;
        }

        let a =
            call_lisp_lisp(env, "nth", vec![0.into_lisp(env).unwrap(), value])?;

        let b =
            call_lisp_lisp(env, "nth", vec![1.into_lisp(env).unwrap(), value])?;
        FromVecOfLisp::from_vec_of_lisp(env, vec![a, b])
    }
}

impl<A: FromLisp, B: FromLisp, C: FromLisp> FromLisp for (A, B, C) {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<(A, B, C)> {
        if !call_lisp_rust::<&str, bool>(env, "listp", vec![value])? {
            return None;
        }

        if call_lisp_rust::<&str, i64>(env, "length", vec![value])? != 3 {
            return None;
        }

        let a =
            call_lisp_lisp(env, "nth", vec![0.into_lisp(env).unwrap(), value])?;
        let b =
            call_lisp_lisp(env, "nth", vec![1.into_lisp(env).unwrap(), value])?;
        let c =
            call_lisp_lisp(env, "nth", vec![2.into_lisp(env).unwrap(), value])?;
        FromVecOfLisp::from_vec_of_lisp(env, vec![a, b, c])
    }
}

impl<A: FromLisp, B: FromLisp, C: FromLisp, D: FromLisp, E: FromLisp> FromLisp
    for (A, B, C, D, E)
{
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<(A, B, C, D, E)> {
        if !call_lisp_rust::<&str, bool>(env, "listp", vec![value])? {
            return None;
        }

        if call_lisp_rust::<&str, i64>(env, "length", vec![value])? != 5 {
            return None;
        }

        let a =
            call_lisp_rust(env, "nth", vec![0.into_lisp(env).unwrap(), value])?;
        let b =
            call_lisp_rust(env, "nth", vec![1.into_lisp(env).unwrap(), value])?;
        let c =
            call_lisp_rust(env, "nth", vec![2.into_lisp(env).unwrap(), value])?;
        let d =
            call_lisp_rust(env, "nth", vec![3.into_lisp(env).unwrap(), value])?;
        let e =
            call_lisp_rust(env, "nth", vec![4.into_lisp(env).unwrap(), value])?;
        Some((a, b, c, d, e))
    }
}

impl<T: FromLisp> FromLisp for Vec<T> {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<Vec<T>> {
        if !call_lisp_rust::<&str, bool>(env, "listp", vec![value])? {
            return None;
        }

        let len = call_lisp_rust::<&str, i64>(env, "length", vec![value])?;

        let mut vec = Vec::new();

        for i in 0..len {
            let i = i.into_lisp(env)?;
            let element = call_lisp_rust(env, "nth", vec![i, value])?;
            vec.push(element);
        }

        Some(vec)
    }
}

impl<T: FromLisp> FromLisp for Option<T> {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> Option<Option<T>> {
        if call_lisp_rust(env, "null", vec![value])? {
            Some(None)
        } else {
            T::from_lisp(env, value).map(|v| Some(v))
        }
    }
}

// FromVecOfLisp

pub trait FromVecOfLisp {
    unsafe fn from_vec_of_lisp(
        env: *mut emacs_env,
        vec_of_lisp: Vec<emacs_value>,
    ) -> Option<Self>
    where
        Self: Sized;
}

impl FromVecOfLisp for () {
    unsafe fn from_vec_of_lisp(
        _env: *mut emacs_env,
        _vec_of_lisp: Vec<emacs_value>,
    ) -> Option<()> {
        Some(())
    }
}

impl<A: FromLisp, B: FromLisp> FromVecOfLisp for (A, B) {
    unsafe fn from_vec_of_lisp(
        env: *mut emacs_env,
        vec_of_lisp: Vec<emacs_value>,
    ) -> Option<(A, B)> {
        let a = FromLisp::from_lisp(env, vec_of_lisp[0])?;
        let b = FromLisp::from_lisp(env, vec_of_lisp[1])?;
        Some((a, b))
    }
}

impl<A: FromLisp, B: FromLisp, C: FromLisp> FromVecOfLisp for (A, B, C) {
    unsafe fn from_vec_of_lisp(
        env: *mut emacs_env,
        vec_of_lisp: Vec<emacs_value>,
    ) -> Option<(A, B, C)> {
        // todo: take nargs into account
        let a = FromLisp::from_lisp(env, vec_of_lisp[0])?;
        let b = FromLisp::from_lisp(env, vec_of_lisp[1])?;
        let c = FromLisp::from_lisp(env, vec_of_lisp[2])?;
        Some((a, b, c))
    }
}
