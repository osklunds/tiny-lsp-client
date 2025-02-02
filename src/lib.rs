// Heavily inspired by https://github.com/zbelial/lspce
// and https://ryanfaulhaber.com/posts/first-emacs-module-rust/

#![allow(warnings)] 

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

mod learning_tests;
mod dummy;
mod connection;
mod message;

use crate::connection::Connection;

use std::ffi::CString;
use std::os::raw;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::Once;

#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;

// todo: stolen from lspce. Understand how, and maybe make safer
fn connections() -> &'static Arc<Mutex<HashMap<String, Connection>>> {
    static mut CONNECTIONS: MaybeUninit<Arc<Mutex<HashMap<String, Connection>>>> =
        MaybeUninit::uninit();
    static ONCE: Once = Once::new();

    ONCE.call_once(|| unsafe {
        CONNECTIONS.as_mut_ptr().write(Arc::new(Mutex::new(HashMap::new())))
    });

    unsafe { &*CONNECTIONS.as_mut_ptr() }
}


#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(ert: *mut emacs_runtime) -> libc::c_int {
    let env = (*ert).get_environment.unwrap()(ert);

    let make_function = (*env).make_function.unwrap();
    let intern = (*env).intern.unwrap();
    let funcall = (*env).funcall.unwrap();
    let fset = intern(env, CString::new("fset").unwrap().as_ptr());

    let tlc__rust_all_server_info = make_function(
        env,
        0,
        0,
        Some(tlc__rust_all_server_info),
        CString::new("doc todo").unwrap().as_ptr(),
        std::ptr::null_mut(),
    );
    let tlc__rust_all_server_info_sym = intern(
        env,
        CString::new("tlc--rust-all-server-info").unwrap().as_ptr()
    );
    funcall(
        env,
        fset,
        2,
        [tlc__rust_all_server_info_sym, tlc__rust_all_server_info].as_mut_ptr()
    );

    let tlc__rust_start_server = make_function(
        env,
        2,
        2,
        Some(tlc__rust_start_server),
        CString::new("doc todo").unwrap().as_ptr(),
        std::ptr::null_mut(),
    );
    let tlc__rust_start_server_sym = intern(
        env,
        CString::new("tlc--rust-start-server").unwrap().as_ptr()
    );
    funcall(
        env,
        fset,
        2,
        [tlc__rust_start_server_sym, tlc__rust_start_server].as_mut_ptr()
    );
    
        
        



    // unclear if/why needed
    let provide = intern(env, CString::new("provide").unwrap().as_ptr());
    let feat_name = intern(env, CString::new("my-rust-mod").unwrap().as_ptr());
    let provide_args = [feat_name].as_mut_ptr();
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

unsafe extern "C" fn tlc__rust_all_server_info(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let make_integer = (*env).make_integer.unwrap();
    let a = make_integer(env, 32);
    let b = make_integer(env, 8);
    let c = make_integer(env, 9);

    let intern = (*env).intern.unwrap();
    let list = intern(env, CString::new("list").unwrap().as_ptr());
    let funcall = (*env).funcall.unwrap();

    let res = funcall(env, list, 3, [a, b, c].as_mut_ptr());

    res
}

unsafe extern "C" fn tlc__rust_start_server(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let intern = (*env).intern.unwrap();
    let list = intern(env, CString::new("list").unwrap().as_ptr());
    let funcall = (*env).funcall.unwrap();
    let copy_string_contents = (*env).copy_string_contents.unwrap();

    let root_uri: emacs_value = *args.offset(0);
    let server_cmd: emacs_value = *args.offset(1);

    let mut conns = connections().lock().unwrap();
    let mut buf = vec![0; 100];
    let mut len = 100;
    let res = copy_string_contents(env, root_uri, buf.as_mut_ptr() as *mut i8, &mut len);
    assert!(res);
    len -= 1;
    let string = std::str::from_utf8(&buf[0..len as usize]).unwrap();

    if conns.contains_key(string) {
        intern(env, CString::new("already-started").unwrap().as_ptr())
    } else {
        let mut connection = Connection::new(
            "rust-analyzer",
            string
        );
        connection.initialize();
        conns.insert(string.to_string(), connection);
        intern(env, CString::new("started").unwrap().as_ptr())
    }
}

unsafe extern "C" fn tlc__rust_send_request(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    todo!()
}

unsafe extern "C" fn tlc__rust_recv_response(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    todo!()
}
