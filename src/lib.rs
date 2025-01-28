// Heavily inspired by https://github.com/zbelial/lspce
// and https://ryanfaulhaber.com/posts/first-emacs-module-rust/

#![allow(warnings)] 

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

mod learning_tests;
mod dummy;
mod connection;
mod message;

use std::ffi::CString;
use std::os::raw;

#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;

#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(ert: *mut emacs_runtime) -> libc::c_int {
    let env = (*ert)
        .get_environment
        .unwrap()
        (ert);

    let make_function = (*env).make_function.unwrap();

    let my_message_func = make_function(
        env,
        0,
        0,
        Some(message_from_rust),
        CString::new("This will print a nice message from Rust code!")
            .unwrap()
            .as_ptr(),
        std::ptr::null_mut(),
    );

    let intern = (*env).intern.expect("could not get intern");

    // creates "my-message-from-rust symbol"
    let my_message_sym = intern(env, CString::new("my-message-from-rust").unwrap().as_ptr());

    // intern will return a symbol if it's already defined.
    // we're going to use `fset` to associate our functions to symbols
    let fset = intern(env, CString::new("fset").unwrap().as_ptr());

    // we also want to actually call fset, so we'll need funcall
    let funcall = (*env).funcall.expect("could not get funcall");

    // in Emacs Lisp this is like:
    // (fset 'my-message-from-rust (lambda () ...))
    funcall(env, fset, 2, [my_message_sym, my_message_func].as_mut_ptr());
    let provide = intern(env, CString::new("provide").unwrap().as_ptr());
    let feat_name = intern(env, CString::new("my-rust-mod").unwrap().as_ptr());
    let provide_args = [feat_name].as_mut_ptr();

    // equivalent of (provide 'my-rust-mod)
    funcall(env, provide, 1, provide_args);

    0
}

unsafe extern "C" fn message_from_rust(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let string = "Hello Emacs! I'm from Rust!126";
    let make_string = (*env).make_string.unwrap();
    let c_string = CString::new(string).unwrap();
    let len = c_string.as_bytes().len() as isize;
    make_string(env, c_string.as_ptr(), len)
}
