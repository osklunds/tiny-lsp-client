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
    })
    .unwrap();
    let symbol = intern(env, symbol).unwrap();
    call_lisp_lisp(env, "fset", vec![symbol, emacs_fun]).unwrap();
}

pub unsafe fn provide_tlc_rust(env: *mut emacs_env) {
    call1_rust(env, "provide", Symbol("tlc-rust".to_string())).unwrap();
}

// Calling emacs functions
// These need to be exported because need to be used in IntoLisp/FromLisp
// impls for Enums. However, it might be the case that those Enums are not
// good since they mean union types from the lisp side are used.

// To be used when calling with lisp arguments and need lisp return value
pub unsafe fn call_lisp_lisp<F: AsRef<str>>(
    env: *mut emacs_env,
    function_name: F,
    mut args: Vec<emacs_value>,
) -> LispResult<emacs_value> {
    let funcall = (*env).funcall.unwrap();
    let function_symbol = intern(env, function_name)?;
    handle_non_local_exit(env, || {
        funcall(env, function_symbol, args.len() as isize, args.as_mut_ptr())
    })
}

// To be used when calling with rust arguments and don't care about return value
unsafe fn call1_rust<F: AsRef<str>, T: IntoLisp>(
    env: *mut emacs_env,
    function_name: F,
    arg1: T,
) -> LispResult<()> {
    let arg1 = arg1.into_lisp(env)?;
    call_lisp_lisp(env, function_name, vec![arg1])?;
    Ok(())
}

// To be used when calling with lisp arguments but need rust return value
pub unsafe fn call_lisp_rust<F: AsRef<str>, T: FromLisp>(
    env: *mut emacs_env,
    func: F,
    args: Vec<emacs_value>,
) -> LispResult<T> {
    let ret = call_lisp_lisp(env, func, args)?;
    T::from_lisp(env, ret)
}

// Lisp function in Rust

pub unsafe fn lisp_function_in_rust<
    A: FromVecOfLisp,
    R: IntoLisp,
    F: FnMut(A) -> R,
    S: AsRef<str> + std::fmt::Display,
>(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    log_args_ret: bool,
    function_name: S,
    mut function: F,
) -> emacs_value {
    let res: LispResult<_> = (|| {
        if log_args_ret {
            log_args(env, nargs, args, &function_name)?;
        }
        let args_vec = args_pointer_to_args_vec(nargs, args);
        let arg = FromVecOfLisp::from_vec_of_lisp(env, args_vec)?;
        let ret = function(arg).into_lisp(env)?;
        if log_args_ret {
            log_lisp_value(
                env,
                format!("{} return value: '%S'", &function_name),
                ret,
            )?;
        }
        Ok(ret)
    })();
    // If not Ok, check the status code. Err() can be returned either because a
    // call to emacs failed, in which case there is a status, handled
    // automatically by handle_non_local_exit(). But it can also happen manually
    // if e.g. the number of elements in a list is detected to be wrong. In the
    // manual cases a signal should be raised using the provided error message
    // string. handle_non_local_exit() is the only placed allowed to use
    // Err(""), i.e. empty stirng, because an error is already present.

    match res {
        Ok(ret) => ret,
        Err(error_message) => {
            let status = (*env).non_local_exit_check.unwrap()(env);
            if status == emacs_funcall_exit_emacs_funcall_exit_return {
                if let Ok(error_symbol) =
                    (Symbol("error".to_string())).into_lisp(env)
                {
                    if let Ok(data) = vec![error_message].into_lisp(env) {
                        (*env).non_local_exit_signal.unwrap()(
                            env,
                            error_symbol,
                            data,
                        );
                    }
                }
            }
            // If the two calls above failed, a signal is raised anyway

            // This return value doesn't matter since a signal is raised
            (*env).intern.unwrap()(env, "nil".as_ptr() as *const i8)
        }
    }
}

unsafe fn log_args<S: AsRef<str>>(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    function_name: S,
) -> LispResult<()> {
    // logger::log_rust_debug! already knows whether to log or not. But check
    // anyway as an optimization so that lots of string and terms aren't
    // created unecessarily.
    // Idea: pass lambda that is lazily called. So can do a general
    // optimization without macros
    // todo: use log_lisp_value
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
    Ok(())
}

unsafe fn log_lisp_value<S: IntoLisp>(
    env: *mut emacs_env,
    string: S,
    value: emacs_value,
) -> LispResult<()> {
    if logger::is_log_enabled!(LOG_RUST_DEBUG) {
        let formatted: String =
            call_lisp_rust(env, "format", vec![string.into_lisp(env)?, value])?;
        logger::log_rust_debug!("{}", formatted);
    }
    Ok(())
}

unsafe fn args_pointer_to_args_vec(
    nargs: isize,
    args: *mut emacs_value,
) -> Vec<emacs_value> {
    let mut args_list = Vec::new();
    for i in 0..nargs {
        args_list.push(*args.offset(i));
    }
    args_list
}

unsafe fn handle_non_local_exit<F: FnMut() -> R, R>(
    env: *mut emacs_env,
    mut func: F,
) -> LispResult<R> {
    let result = func();
    let status = (*env).non_local_exit_check.unwrap()(env);
    if status == emacs_funcall_exit_emacs_funcall_exit_return {
        Ok(result)
    } else {
        // logger can only be called once the log file has been initialized,
        // which is not the case in emacs_module_init for example. But for
        // now, gamble that no non-local exists until log file has been
        // created.
        logger::log_rust_debug!("non local exit: {}", status);
        Err("".to_string())
    }
}

unsafe fn intern<T: AsRef<str>>(
    env: *mut emacs_env,
    symbol: T,
) -> LispResult<emacs_value> {
    let symbol = CString::new(symbol.as_ref()).unwrap();
    handle_non_local_exit(env, || (*env).intern.unwrap()(env, symbol.as_ptr()))
}

// IntoLisp

// @credits: IntoLisp/FromLisp inspired by
// https://github.com/ubolonton/emacs-module-rs
pub trait IntoLisp {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value>;
}

pub struct Symbol(pub String);

impl IntoLisp for Symbol {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        intern(env, &self.0)
    }
}

impl FromLisp for Symbol {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<Symbol> {
        let string = call_lisp_rust(env, "symbol-name", vec![value])?;
        Ok(Symbol(string))
    }
}

impl IntoLisp for String {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        AsRef::<str>::as_ref(&self).into_lisp(env)
    }
}

impl IntoLisp for &String {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        AsRef::<str>::as_ref(&self).into_lisp(env)
    }
}

impl IntoLisp for &str {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        let make_string = (*env).make_string.unwrap();
        let c_string = CString::new(self).unwrap();
        let len = c_string.as_bytes().len() as isize;
        let c_string_ptr = c_string.as_ptr();
        handle_non_local_exit(env, || make_string(env, c_string_ptr, len))
    }
}

macro_rules! impl_into_lisp_for_integer {
    ($t:ident) => {
        impl IntoLisp for $t {
            unsafe fn into_lisp(
                self,
                env: *mut emacs_env,
            ) -> LispResult<emacs_value> {
                handle_non_local_exit(env, || {
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
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        if self {
            intern(env, "t")
        } else {
            intern(env, "nil")
        }
    }
}

impl<T: IntoLisp> IntoLisp for Vec<T> {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        let len = self.len();
        let vec_of_lisp_objects: Vec<_> = self
            .into_iter()
            .map_while(|element| element.into_lisp(env).ok())
            .collect();

        if vec_of_lisp_objects.len() == len {
            call_lisp_lisp(env, "list", vec_of_lisp_objects)
        } else {
            Err("IntoLisp for Vec failed".to_string())
        }
    }
}

impl IntoLisp for () {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        false.into_lisp(env)
    }
}

impl<A: IntoLisp, B: IntoLisp> IntoLisp for (A, B) {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        let (a, b) = self;
        let a = a.into_lisp(env)?;
        let b = b.into_lisp(env)?;
        call_lisp_lisp(env, "list", vec![a, b])
    }
}

impl<A: IntoLisp, B: IntoLisp, C: IntoLisp> IntoLisp for (A, B, C) {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
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
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
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
    ) -> LispResult<Self>
    where
        Self: Sized;
}

impl FromLisp for String {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        val: emacs_value,
    ) -> LispResult<String> {
        let copy_string_contents = (*env).copy_string_contents.unwrap();

        // First find the length
        let mut len: isize = 0;
        if !handle_non_local_exit(env, || {
            copy_string_contents(env, val, ptr::null_mut::<i8>(), &mut len)
        })? {
            return Err("Find length FromLisp for String".to_string());
        }
        if len <= 0 {
            return Err("Length < 0 FromLisp for String".to_string());
        }

        // Then get the actual string
        let mut buf = vec![0; len as usize];
        if !handle_non_local_exit(env, || {
            copy_string_contents(
                env,
                val,
                buf.as_mut_ptr() as *mut i8,
                &mut len,
            )
        })? {
            return Err("Get actual string FromLisp for String".to_string());
        }

        len -= 1; // remove null-terminator
        Ok(str::from_utf8(&buf[0..len as usize]).unwrap().to_string())
    }
}

impl FromLisp for bool {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<bool> {
        let null: Symbol = call_lisp_rust(env, "null", vec![value])?;
        Ok(null.0 != "t")
    }
}

macro_rules! impl_from_lisp_for_integer {
    ($t:ident) => {
        impl FromLisp for $t {
            unsafe fn from_lisp(
                env: *mut emacs_env,
                value: emacs_value,
            ) -> LispResult<Self> {
                handle_non_local_exit(env, || {
                    (*env).extract_integer.unwrap()(env, value) as Self
                })
            }
        }
    };
}

impl_from_lisp_for_integer!(i32);
impl_from_lisp_for_integer!(i64);
impl_from_lisp_for_integer!(u64);
impl_from_lisp_for_integer!(usize);

impl<A: FromLisp> FromLisp for (A,) {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<(A,)> {
        check_tuple(env, value, 1)?;

        let a =
            call_lisp_lisp(env, "nth", vec![0.into_lisp(env).unwrap(), value])?;

        FromVecOfLisp::from_vec_of_lisp(env, vec![a])
    }
}

impl<A: FromLisp, B: FromLisp> FromLisp for (A, B) {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<(A, B)> {
        check_tuple(env, value, 2)?;

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
    ) -> LispResult<(A, B, C)> {
        check_tuple(env, value, 3)?;

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
    ) -> LispResult<(A, B, C, D, E)> {
        check_tuple(env, value, 5)?;

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
        Ok((a, b, c, d, e))
    }
}

unsafe fn check_tuple(
    env: *mut emacs_env,
    value: emacs_value,
    exp_arity: i64,
) -> LispResult<()> {
    if !call_lisp_rust::<&str, bool>(env, "listp", vec![value])? {
        return Err(format!(
            "In check_tuple, exp_arity: {}, but not a list",
            exp_arity
        ));
    }

    let arity = call_lisp_rust::<&str, i64>(env, "length", vec![value])?;
    if exp_arity != arity {
        return Err(format!(
            "In check_tuple, exp_arity: {}, arity: {}",
            exp_arity, arity
        ));
    }
    Ok(())
}

impl<T: FromLisp> FromLisp for Vec<T> {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<Vec<T>> {
        if !call_lisp_rust::<&str, bool>(env, "listp", vec![value])? {
            return Err("FromLisp for Vec, error in listp".to_string());
        }

        let len = call_lisp_rust::<&str, i64>(env, "length", vec![value])?;

        let mut vec = Vec::new();

        for i in 0..len {
            let i = i.into_lisp(env)?;
            let element = call_lisp_rust(env, "nth", vec![i, value])?;
            vec.push(element);
        }

        Ok(vec)
    }
}

impl<T: FromLisp> FromLisp for Option<T> {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<Option<T>> {
        if call_lisp_rust(env, "null", vec![value])? {
            Ok(None)
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
    ) -> LispResult<Self>
    where
        Self: Sized;
}

impl FromVecOfLisp for () {
    unsafe fn from_vec_of_lisp(
        _env: *mut emacs_env,
        _vec_of_lisp: Vec<emacs_value>,
    ) -> LispResult<()> {
        Ok(())
    }
}

impl<A: FromLisp> FromVecOfLisp for (A,) {
    unsafe fn from_vec_of_lisp(
        env: *mut emacs_env,
        vec_of_lisp: Vec<emacs_value>,
    ) -> LispResult<(A,)> {
        if vec_of_lisp.len() == 1 {
            let a = FromLisp::from_lisp(env, vec_of_lisp[0])?;
            Ok((a,))
        } else {
            Err("FromVecOfLisp for (A,)".to_string())
        }
    }
}

impl<A: FromLisp, B: FromLisp> FromVecOfLisp for (A, B) {
    unsafe fn from_vec_of_lisp(
        env: *mut emacs_env,
        vec_of_lisp: Vec<emacs_value>,
    ) -> LispResult<(A, B)> {
        if vec_of_lisp.len() == 2 {
            let a = FromLisp::from_lisp(env, vec_of_lisp[0])?;
            let b = FromLisp::from_lisp(env, vec_of_lisp[1])?;
            Ok((a, b))
        } else {
            Err("FromVecOfLisp for (A,B)".to_string())
        }
    }
}

impl<A: FromLisp, B: FromLisp, C: FromLisp> FromVecOfLisp for (A, B, C) {
    unsafe fn from_vec_of_lisp(
        env: *mut emacs_env,
        vec_of_lisp: Vec<emacs_value>,
    ) -> LispResult<(A, B, C)> {
        if vec_of_lisp.len() == 3 {
            let a = FromLisp::from_lisp(env, vec_of_lisp[0])?;
            let b = FromLisp::from_lisp(env, vec_of_lisp[1])?;
            let c = FromLisp::from_lisp(env, vec_of_lisp[2])?;
            Ok((a, b, c))
        } else {
            Err("FromVecOfLisp for (A,B,C)".to_string())
        }
    }
}

// Result

// So that "must use" warnings are emitted
pub type LispResult<T> = Result<T, String>;
