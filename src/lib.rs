// Heavily inspired by https://github.com/zbelial/lspce
// and https://ryanfaulhaber.com/posts/first-emacs-module-rust/

#![allow(warnings)] 

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

mod learning_tests;
mod dummy;
mod connection;
mod message;

use crate::connection::Connection;
use crate::message::*;

use std::ffi::CString;
use std::os::raw;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::Once;
use std::fs;

macro_rules! c_string {
    ($x:expr) => {
        CString::new($x).unwrap().as_ptr()
    };
}

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

    export_function(
        env,
        0,
        0,
        tlc__rust_all_server_info,
        "doc todo",
        "tlc--rust-all-server-info"
    );

    export_function(
        env,
        2,
        2,
        tlc__rust_start_server,
        "doc todo",
        "tlc--rust-start-server"
    );

    export_function(
        env,
        3,
        3,
        tlc__rust_send_request,
        "doc todo",
        "tlc--rust-send-request"
    );

    export_function(
        env,
        1,
        1,
        tlc__rust_recv_response,
        "doc todo",
        "tlc--rust-recv-response"
    );

    export_function(
        env,
        3,
        3,
        tlc__rust_send_notification,
        "doc todo",
        "tlc--rust-send-notification"
    );

    let feat_name = intern(env, c_string!("tlc-rust"));
    call(env, "provide", vec![feat_name]);

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


    call(env, "list", vec![a, b, c])
}

unsafe extern "C" fn tlc__rust_start_server(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let intern = (*env).intern.unwrap();
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
        intern(env, c_string!("already-started"))
    } else {
        let mut connection = Connection::new(
            "rust-analyzer",
            string
        );
        connection.initialize();
        conns.insert(string.to_string(), connection);
        intern(env, c_string!("started"))
    }
}

unsafe extern "C" fn tlc__rust_send_request(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let intern = (*env).intern.unwrap();
    let make_integer = (*env).make_integer.unwrap();
    let extract_integer = (*env).extract_integer.unwrap();

    let root_uri = extract_string(env, *args.offset(0));
    let mut conns = connections().lock().unwrap();
    let mut connection = &mut conns.get_mut(&root_uri).unwrap();

    let request_type = extract_string(env, *args.offset(1));
    if request_type == "textDocument/definition" {
        let request_args = *args.offset(2);

        let uri = call(env, "nth", vec![make_integer(env, 0), request_args]);
        let uri = extract_string(env, uri);
        let uri = "file://".to_owned() + &uri;

        let character =
            call(env, "nth", vec![make_integer(env, 1), request_args]);
        let character = extract_integer(env, character) as usize;

        let line =
            call(env, "nth", vec![make_integer(env, 2), request_args]);
        let line = extract_integer(env, line) as usize;

        let params = RequestParams::DefinitionParams( DefinitionParams {
            text_document: TextDocumentIdentifier {
                uri,
            },
            position: Position {
                character,
                line,
            }
        });
        connection.send_request(request_type, params);
        intern(env, c_string!("ok"))
    } else {
        panic!("Incorrect request type")
    }
}

unsafe extern "C" fn tlc__rust_send_notification(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let intern = (*env).intern.unwrap();
    let nth = intern(env, c_string!("nth"));
    let make_integer = (*env).make_integer.unwrap();
    let extract_integer = (*env).extract_integer.unwrap();

    let root_uri = extract_string(env, *args.offset(0));
    let mut conns = connections().lock().unwrap();
    let mut connection = &mut conns.get_mut(&root_uri).unwrap();

    let request_type = extract_string(env, *args.offset(1));
    if request_type == "textDocument/didOpen" {
        let request_args = *args.offset(2);

        let uri = call(env, "nth", vec![make_integer(env, 0), request_args]);
        let uri = extract_string(env, uri);
        let text = fs::read_to_string(&uri).unwrap();
        let uri = "file://".to_owned() + &uri;

        let language_id = "rust".to_string();
        let version = 1;

        connection.send_notification(
            "textDocument/didOpen".to_string(),
            NotificationParams::DidOpenTextDocumentParams(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri,
                    language_id,
                    version,
                    text
                }
            }));

        intern(env, c_string!("ok"))
    } else {
        panic!("Incorrect request type")
    }
}

unsafe extern "C" fn tlc__rust_recv_response(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let intern = (*env).intern.unwrap();
    let make_integer = (*env).make_integer.unwrap();

    let root_uri = extract_string(env, *args.offset(0));
    let mut conns = connections().lock().unwrap();
    let mut connection = &mut conns.get_mut(&root_uri).unwrap();

    if let Some(response) = connection.try_recv_response() {
        if let Some(result) = response.result {
            if let Result::TextDocumentDefinitionResult(definition_result) = result {
                let DefinitionResult::LocationLinkList(location_link_list) =
                    definition_result;
                if !location_link_list.is_empty() {
                let location_link = &location_link_list[0];
                let uri = &location_link.target_uri;
                    let range = &location_link.target_selection_range;

                    call(env,
                         "list",
                         vec![make_string(env, uri.to_string()),
                              make_integer(env, range.start.line as i64),
                              make_integer(env, range.start.character as i64),
                              make_integer(env, range.end.line as i64),
                              make_integer(env, range.end.character as i64),
                         ]
                    )
                } else {
                    intern(env, c_string!("other-response"))
                }
            } else {
                intern(env, c_string!("other-response"))
            }
        } else {
            intern(env, c_string!("other-response"))
        }
    } else {
        intern(env, c_string!("no-response"))
    }
}

unsafe fn extract_string(env: *mut emacs_env, val: emacs_value) -> String {
    let copy_string_contents = (*env).copy_string_contents.unwrap();
    let mut buf = vec![0; 1000];
    let mut len = 1000;
    let res = copy_string_contents(env, val, buf.as_mut_ptr() as *mut i8, &mut len);
    assert!(res);
    len -= 1;
    std::str::from_utf8(&buf[0..len as usize]).unwrap().to_string()
}

unsafe fn make_string(env: *mut emacs_env, string: String) -> emacs_value {
    let make_string = (*env).make_string.unwrap();
    let c_string = CString::new(string).unwrap();
    let len = c_string.as_bytes().len() as isize;
    make_string(env, c_string.as_ptr(), len)
}

unsafe fn call<F: AsRef<str>>(env: *mut emacs_env,
                              func: F,
                              mut args: Vec<emacs_value>
) -> emacs_value {
    let intern = (*env).intern.unwrap();
    let funcall = (*env).funcall.unwrap();
    funcall(env,
            intern(env, c_string!(func.as_ref())),
            args.len() as isize,
            args.as_mut_ptr()
            )
}

unsafe fn export_function(env: *mut emacs_env,
                          min_arity: isize,
                          max_arity: isize,
                          fun: unsafe extern "C" fn(
                              env: *mut emacs_env,
                              nargs: isize,
                              args: *mut emacs_value,
                              data: *mut ::std::os::raw::c_void,
                          ) -> emacs_value,
                          docstring: &str,
                          symbol: &str
) {
    let make_function = (*env).make_function.unwrap();
    let intern = (*env).intern.unwrap();

    let emacs_fun = make_function(
        env,
        min_arity,
        max_arity,
        Some(fun),
        c_string!(docstring),
        std::ptr::null_mut(),
    );
    let symbol = intern(
        env,
        c_string!(symbol)
    );
    call(env, "fset", vec![symbol, emacs_fun]);
}
                          

