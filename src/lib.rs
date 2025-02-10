// Heavily inspired by https://github.com/zbelial/lspce
// and https://ryanfaulhaber.com/posts/first-emacs-module-rust/

#![allow(warnings)]

#[rustfmt::skip]
mod dummy;
mod connection;
mod emacs;
mod learning_tests;
mod message;
mod logger;

use crate::connection::Connection;
use crate::emacs::*;
use crate::message::*;

use std::collections::HashMap;
use std::fs;
use std::mem::MaybeUninit;
use std::os::raw;
use std::path::Path;
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

    logger::set_log_file_name("/home/oskar/Downloads/tiny-lsp-client-log.txt");

    0
}

unsafe extern "C" fn tlc__rust_all_server_info(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let mut server_info_list = Vec::new();
    let connections = connections().lock().unwrap();

    for (root_path, connection) in connections.iter() {
        let info = call(
            env,
            "list",
            vec![
                make_string(env, root_path),
                make_string(env, connection.get_command()),
                make_integer(env, connection.get_server_process_id() as i64),
            ],
        );
        server_info_list.push(info);
    }

    call(env, "list", server_info_list)
}

unsafe extern "C" fn tlc__rust_start_server(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let root_path = check_path(extract_string(env, *args.offset(0)));
    let server_cmd = extract_string(env, *args.offset(1));

    let mut connections = connections().lock().unwrap();

    if connections.contains_key(&root_path) {
        intern(env, "already-started")
    } else {
        let mut connection = Connection::new(&server_cmd, &root_path);
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
    let root_path = check_path(extract_string(env, *args.offset(0)));
    let mut connections = connections().lock().unwrap();
    let mut connection = &mut connections.get_mut(&root_path).unwrap();

    let request_type = extract_string(env, *args.offset(1));
    let request_args = *args.offset(2);

    let request_params = if request_type == "textDocument/definition" {
        build_text_document_definition(env, request_args, connection)
    } else {
        panic!("Incorrect request type")
    };
    let id = connection.send_request(request_type, request_params);
    make_integer(env, id as i64)
}

unsafe fn build_text_document_definition(
    env: *mut emacs_env,
    request_args: emacs_value,
    connection: &mut Connection,
) -> RequestParams {
    let file_path = nth(env, 0, request_args);
    let file_path = check_path(extract_string(env, file_path));
    let uri = file_path_to_uri(file_path);

    let line = nth(env, 1, request_args);
    let line = extract_integer(env, line) as usize;

    let character = nth(env, 2, request_args);
    let character = extract_integer(env, character) as usize;

    RequestParams::DefinitionParams(DefinitionParams {
        text_document: TextDocumentIdentifier { uri },
        position: Position { line, character },
    })
}

unsafe extern "C" fn tlc__rust_send_notification(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let root_path = check_path(extract_string(env, *args.offset(0)));
    let mut connections = connections().lock().unwrap();
    let mut connection = &mut connections.get_mut(&root_path).unwrap();

    let request_type = extract_string(env, *args.offset(1));
    let request_args = *args.offset(2);

    let notification_params = if request_type == "textDocument/didOpen" {
        build_text_document_did_open(env, request_args, connection)
    } else if request_type == "textDocument/didChange" {
        build_text_document_did_change(env, request_args, connection)
    } else if request_type == "textDocument/didClose" {
        build_text_document_did_close(env, request_args, connection)
    } else {
        panic!("Incorrect request type")
    };
    connection.send_notification(request_type, notification_params);

    intern(env, "ok")
}

unsafe fn build_text_document_did_open(
    env: *mut emacs_env,
    request_args: emacs_value,
    connection: &mut Connection,
) -> NotificationParams {
    let file_path = nth(env, 0, request_args);
    let file_path = check_path(extract_string(env, file_path));
    let file_content = fs::read_to_string(&file_path).unwrap();
    let uri = file_path_to_uri(file_path);

    NotificationParams::DidOpenTextDocumentParams(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri,
            language_id: LANGUAGE_ID.to_string(),
            version: connection.inc_and_get_version_number(),
            text: file_content,
        },
    })
}

unsafe fn build_text_document_did_change(
    env: *mut emacs_env,
    request_args: emacs_value,
    connection: &mut Connection,
) -> NotificationParams {
    let file_path = nth(env, 0, request_args);
    let file_path = check_path(extract_string(env, file_path));
    let file_content = fs::read_to_string(&file_path).unwrap();
    let uri = file_path_to_uri(file_path);

    let content_changes = nth(env, 1, request_args);
    let content_changes_len = call(env, "length", vec![content_changes]);
    let content_changes_len = extract_integer(env, content_changes_len);

    let mut json_content_changes = Vec::new();

    for i in 0..content_changes_len {
        let content_change = nth(env, i, content_changes);

        let start_line = nth(env, 0, content_change);
        let start_character = nth(env, 1, content_change);
        let end_line = nth(env, 2, content_change);
        let end_character = nth(env, 3, content_change);
        let text = nth(env, 4, content_change);

        let json_content_change = TextDocumentContentChangeEvent {
            range: Range {
                start: Position {
                    line: extract_integer(env, start_line) as usize,
                    character: extract_integer(env, start_character) as usize,
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

    NotificationParams::DidChangeTextDocumentParams(
        DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri,
                version: connection.inc_and_get_version_number(),
            },
            content_changes: json_content_changes,
        },
    )
}

unsafe fn build_text_document_did_close(
    env: *mut emacs_env,
    request_args: emacs_value,
    connection: &mut Connection,
) -> NotificationParams {
    let file_path = nth(env, 0, request_args);
    let file_path = check_path(extract_string(env, file_path));
    let file_content = fs::read_to_string(&file_path).unwrap();
    let uri = file_path_to_uri(file_path);

    NotificationParams::DidCloseTextDocumentParams(DidCloseTextDocumentParams {
        text_document: TextDocumentIdentifier { uri },
    })
}

unsafe extern "C" fn tlc__rust_recv_response(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut raw::c_void,
) -> emacs_value {
    let root_path = check_path(extract_string(env, *args.offset(0)));
    let mut connections = connections().lock().unwrap();
    let mut connection = &mut connections.get_mut(&root_path).unwrap();

    if let Some(response) = connection.try_recv_response() {
        if let Some(result) = response.result {
            if let Result::TextDocumentDefinitionResult(definition_result) =
                result
            {
                let DefinitionResult::LocationLinkList(location_link_list) =
                    definition_result;
                let mut lisp_location_list_vec = Vec::new();

                for location_link in location_link_list {
                    let uri = &location_link.target_uri;
                    let range = &location_link.target_selection_range;

                    let lisp_location = call(
                        env,
                        "list",
                        vec![
                            make_string(env, check_path(uri_to_file_path(uri))),
                            make_integer(env, range.start.line as i64),
                            make_integer(env, range.start.character as i64),
                        ],
                    );
                    lisp_location_list_vec.push(lisp_location);
                }
                let lisp_location_list =
                    call(env, "list", lisp_location_list_vec);
                let id = make_integer(env, response.id as i64);
                call(
                    env,
                    "list",
                    vec![intern(env, "ok"), id, lisp_location_list],
                )
            } else {
                intern(env, "error")
            }
        } else {
            // Now response.error should be Some, but since no details are
            // returned anyway, no need to unwrap and risk crash
            intern(env, "error")
        }
    } else {
        intern(env, "no-response")
    }
}

fn check_path<S: AsRef<str>>(file_path: S) -> S {
    assert!(Path::new(file_path.as_ref()).is_absolute());

    file_path
}

fn file_path_to_uri<S: AsRef<str>>(file_path: S) -> String {
    format!("file://{}", file_path.as_ref())
}

fn uri_to_file_path<S: AsRef<str>>(uri: S) -> String {
    let (first, last) = uri.as_ref().split_at(7);
    assert_eq!("file://", first);
    last.to_string()
}
