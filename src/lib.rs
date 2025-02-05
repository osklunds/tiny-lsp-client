// Heavily inspired by https://github.com/zbelial/lspce
// and https://ryanfaulhaber.com/posts/first-emacs-module-rust/

#![allow(warnings)]

#[rustfmt::skip]
mod dummy;
mod connection;
mod emacs;
mod learning_tests;
mod message;

use crate::connection::Connection;
use crate::emacs::*;
use crate::message::*;

use std::collections::HashMap;
use std::fs;
use std::mem::MaybeUninit;
use std::os::raw;
use std::sync::Once;
use std::sync::{Arc, Mutex};

#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;

// todo: stolen from lspce. Understand how, and maybe make safer
fn connections() -> &'static Arc<Mutex<HashMap<String, Connection>>> {
    static mut CONNECTIONS: MaybeUninit<
        Arc<Mutex<HashMap<String, Connection>>>,
    > = MaybeUninit::uninit();
    static ONCE: Once = Once::new();

    ONCE.call_once(|| unsafe {
        CONNECTIONS
            .as_mut_ptr()
            .write(Arc::new(Mutex::new(HashMap::new())))
    });

    unsafe { &*CONNECTIONS.as_mut_ptr() }
}

#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(
    ert: *mut emacs_runtime,
) -> libc::c_int {
    let env = (*ert).get_environment.unwrap()(ert);

    export_function(
        env,
        0,
        0,
        tlc__rust_all_server_info,
        "doc todo",
        "tlc--rust-all-server-info",
    );

    export_function(
        env,
        2,
        2,
        tlc__rust_start_server,
        "doc todo",
        "tlc--rust-start-server",
    );

    export_function(
        env,
        3,
        3,
        tlc__rust_send_request,
        "doc todo",
        "tlc--rust-send-request",
    );

    export_function(
        env,
        1,
        1,
        tlc__rust_recv_response,
        "doc todo",
        "tlc--rust-recv-response",
    );

    export_function(
        env,
        3,
        3,
        tlc__rust_send_notification,
        "doc todo",
        "tlc--rust-send-notification",
    );

    call(env, "provide", vec![intern(env, "tlc-rust")]);

    0
}

unsafe extern "C" fn tlc__rust_all_server_info(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
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
    let root_path = extract_string(env, *args.offset(0));
    let server_cmd = extract_string(env, *args.offset(1));

    let mut connections = connections().lock().unwrap();

    if connections.contains_key(&root_path) {
        intern(env, "already-started")
    } else {
        let mut connection = Connection::new("rust-analyzer", &root_path);
        connection.initialize();
        connections.insert(root_path.to_string(), connection);
        intern(env, "started")
    }
}

unsafe extern "C" fn tlc__rust_send_request(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let root_path = extract_string(env, *args.offset(0));
    let mut connections = connections().lock().unwrap();
    let mut connection = &mut connections.get_mut(&root_path).unwrap();

    let request_type = extract_string(env, *args.offset(1));
    if request_type == "textDocument/definition" {
        let request_args = *args.offset(2);

        let file_path =
            call(env, "nth", vec![make_integer(env, 0), request_args]);
        let file_path = extract_string(env, file_path);
        let uri = file_path_to_uri(file_path);

        let line = call(env, "nth", vec![make_integer(env, 1), request_args]);
        let line = extract_integer(env, line) as usize;

        let character =
            call(env, "nth", vec![make_integer(env, 2), request_args]);
        let character = extract_integer(env, character) as usize;

        let params = RequestParams::DefinitionParams(DefinitionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position { line, character },
        });
        connection.send_request(request_type, params);
        intern(env, "ok")
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
    let root_path = extract_string(env, *args.offset(0));
    let mut connections = connections().lock().unwrap();
    let mut connection = &mut connections.get_mut(&root_path).unwrap();

    let request_type = extract_string(env, *args.offset(1));
    if request_type == "textDocument/didOpen" {
        let request_args = *args.offset(2);

        let file_path =
            call(env, "nth", vec![make_integer(env, 0), request_args]);
        let file_path = extract_string(env, file_path);
        let file_content = fs::read_to_string(&file_path).unwrap();
        let uri = file_path_to_uri(file_path);

        let language_id = "rust".to_string();
        let version = 1;

        connection.send_notification(
            request_type,
            NotificationParams::DidOpenTextDocumentParams(
                DidOpenTextDocumentParams {
                    text_document: TextDocumentItem {
                        uri,
                        language_id,
                        version,
                        text: file_content,
                    },
                },
            ),
        );
    } else if request_type == "textDocument/didChange" {
        let request_args = *args.offset(2);

        let file_path =
            call(env, "nth", vec![make_integer(env, 0), request_args]);
        let file_path = extract_string(env, file_path);
        let file_content = fs::read_to_string(&file_path).unwrap();
        let uri = file_path_to_uri(file_path);

        let language_id = "rust".to_string();
        let version = 1;

        let content_changes =
            call(env, "nth", vec![make_integer(env, 1), request_args]);
        let content_changes_len = call(env, "length", vec![content_changes]);
        let content_changes_len = extract_integer(env, content_changes_len);

        let mut json_content_changes = Vec::new();

        for i in 0..content_changes_len {
            let content_change =
                call(env, "nth", vec![make_integer(env, i), content_changes]);

            let start_line =
                call(env, "nth", vec![make_integer(env, 0), content_change]);
            let start_character =
                call(env, "nth", vec![make_integer(env, 1), content_change]);
            let end_line =
                call(env, "nth", vec![make_integer(env, 2), content_change]);
            let end_character =
                call(env, "nth", vec![make_integer(env, 3), content_change]);
            let text =
                call(env, "nth", vec![make_integer(env, 4), content_change]);

            let json_content_change = TextDocumentContentChangeEvent {
                range: Range {
                    start: Position {
                        line: extract_integer(env, start_line) as usize,
                        character: extract_integer(env, start_character)
                            as usize,
                    },
                    end: Position {
                        line: extract_integer(env, end_line) as usize,
                        character: extract_integer(env, end_character) as usize,
                    },
                },
                text: extract_string(env, text),
            };
            json_content_changes.push(json_content_change);
        }

        connection.send_notification(
            request_type,
            NotificationParams::DidChangeTextDocumentParams(
                DidChangeTextDocumentParams {
                    text_document: VersionedTextDocumentIdentifier {
                        uri,
                        version,
                    },
                    content_changes: json_content_changes,
                },
            ),
        );
    } else if request_type == "textDocument/didClose" {
        let request_args = *args.offset(2);

        let file_path =
            call(env, "nth", vec![make_integer(env, 0), request_args]);
        let file_path = extract_string(env, file_path);
        let file_content = fs::read_to_string(&file_path).unwrap();
        let uri = file_path_to_uri(file_path);

        connection.send_notification(
            request_type,
            NotificationParams::DidCloseTextDocumentParams(
                DidCloseTextDocumentParams {
                    text_document: TextDocumentIdentifier { uri },
                },
            ),
        );
    } else {
        panic!("Incorrect request type")
    }

    intern(env, "ok")
}

unsafe extern "C" fn tlc__rust_recv_response(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let root_path = extract_string(env, *args.offset(0));
    let mut connections = connections().lock().unwrap();
    let mut connection = &mut connections.get_mut(&root_path).unwrap();

    if let Some(response) = connection.try_recv_response() {
        if let Some(result) = response.result {
            if let Result::TextDocumentDefinitionResult(definition_result) =
                result
            {
                let DefinitionResult::LocationLinkList(location_link_list) =
                    definition_result;
                if !location_link_list.is_empty() {
                    let location_link = &location_link_list[0];
                    let uri = &location_link.target_uri;
                    let range = &location_link.target_selection_range;

                    call(
                        env,
                        "list",
                        vec![
                            make_string(env, uri.to_string()),
                            make_integer(env, range.start.line as i64),
                            make_integer(env, range.start.character as i64),
                            make_integer(env, range.end.line as i64),
                            make_integer(env, range.end.character as i64),
                        ],
                    )
                } else {
                    intern(env, "other-response")
                }
            } else {
                intern(env, "other-response")
            }
        } else {
            intern(env, "other-response")
        }
    } else {
        intern(env, "no-response")
    }
}

fn file_path_to_uri<S: AsRef<str>>(file_path: S) -> String {
    format!("file://{}", file_path.as_ref())
}
