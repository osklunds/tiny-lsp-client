// Heavily inspired by https://github.com/zbelial/lspce
// and https://ryanfaulhaber.com/posts/first-emacs-module-rust/

#[allow(warnings)]
#[rustfmt::skip]
mod dummy;
mod emacs;
mod logger;
mod message;
mod server;
mod servers;

use crate::emacs::*;
use crate::message::*;
use crate::server::Server;

use std::os::raw;
use std::path::Path;
use std::str;
use std::sync::atomic::Ordering;

#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;

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
        "tlc--rust-all-server-info",
    );

    export_function(
        env,
        2,
        2,
        tlc__rust_start_server,
        "tlc--rust-start-server",
    );

    export_function(
        env,
        3,
        3,
        tlc__rust_send_request,
        "tlc--rust-send-request",
    );

    export_function(
        env,
        1,
        1,
        tlc__rust_recv_response,
        "tlc--rust-recv-response",
    );

    export_function(
        env,
        3,
        3,
        tlc__rust_send_notification,
        "tlc--rust-send-notification",
    );

    export_function(env, 2, 2, tlc__rust_set_option, "tlc--rust-set-option");

    export_function(
        env,
        1,
        1,
        tlc__rust_log_emacs_debug,
        "tlc--rust-log-emacs-debug",
    );

    export_function(env, 1, 1, tlc__rust_stop_server, "tlc--rust-stop-server");

    call(env, "provide", vec![intern(env, "tlc-rust")]);

    0
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_all_server_info(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    log_args(env, nargs, args, "tlc__rust_all_server_info");
    let mut server_info_list = Vec::new();

    servers::with_servers(|servers| {
        for (root_path, server) in servers.iter() {
            let info = call(
                env,
                "list",
                vec![
                    make_string(env, root_path),
                    make_string(env, server.get_command()),
                    make_integer(env, server.get_server_process_id() as i64),
                ],
            );
            server_info_list.push(info);
        }
        call(env, "list", server_info_list)
    })
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_start_server(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    log_args(env, nargs, args, "tlc__rust_start_server");
    let root_path = check_path(extract_string(env, *args.offset(0)));
    let server_cmd = extract_string(env, *args.offset(1));

    servers::with_servers(|servers| {
        if servers.contains_key(&root_path) {
            return intern(env, "already-started");
        } else {
            logger::log_rust_debug!("Need to start new");
        }
        match Server::new(&server_cmd, &root_path) {
            Some(mut server) => match server.initialize() {
                Some(()) => {
                    servers.insert(root_path.to_string(), server);
                    intern(env, "started")
                }
                None => intern(env, "start-failed"),
            },
            None => intern(env, "start-failed"),
        }
    })
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_send_request(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    log_args(env, nargs, args, "tlc__rust_send_request");

    handle_call(env, nargs, args, |env, args, server| {
        let request_type = extract_string(env, args[1]);
        let request_args = args[2];

        let request_params = if request_type == "textDocument/definition" {
            build_text_document_definition(env, request_args, server)
        } else if request_type == "textDocument/completion" {
            build_text_document_completion(env, request_args, server)
        } else {
            panic!("Incorrect request type")
        };
        server
            .send_request(request_type, request_params)
            .map(|id| make_integer(env, id as i64))
    })
}

#[allow(non_snake_case)]
unsafe fn build_text_document_definition(
    env: *mut emacs_env,
    request_args: emacs_value,
    _server: &mut Server,
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

#[allow(non_snake_case)]
unsafe fn build_text_document_completion(
    env: *mut emacs_env,
    request_args: emacs_value,
    _server: &mut Server,
) -> RequestParams {
    let file_path = nth(env, 0, request_args);
    let file_path = check_path(extract_string(env, file_path));
    let uri = file_path_to_uri(file_path);

    let line = nth(env, 1, request_args);
    let line = extract_integer(env, line) as usize;

    let character = nth(env, 2, request_args);
    let character = extract_integer(env, character) as usize;

    RequestParams::CompletionParams(CompletionParams {
        text_document: TextDocumentIdentifier { uri },
        position: Position { line, character },
        context: CompletionContext { trigger_kind: 1 },
    })
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_send_notification(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    log_args(env, nargs, args, "tlc__rust_send_notification");

    handle_call(env, nargs, args, |env, args, server| {
        let request_type = extract_string(env, args[1]);
        let request_args = args[2];

        let notification_params = if request_type == "textDocument/didOpen" {
            build_text_document_did_open(env, request_args, server)
        } else if request_type == "textDocument/didChange" {
            build_text_document_did_change(env, request_args, server)
        } else if request_type == "textDocument/didClose" {
            build_text_document_did_close(env, request_args, server)
        } else {
            panic!("Incorrect request type")
        };
        server
            .send_notification(request_type, notification_params)
            .map(|_| intern(env, "ok"))
    })
}

unsafe fn build_text_document_did_open(
    env: *mut emacs_env,
    request_args: emacs_value,
    server: &mut Server,
) -> NotificationParams {
    let file_path = nth(env, 0, request_args);
    let file_path = check_path(extract_string(env, file_path));
    let uri = file_path_to_uri(file_path);

    let file_content = extract_string(env, nth(env, 1, request_args));

    NotificationParams::DidOpenTextDocumentParams(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri,
            language_id: LANGUAGE_ID.to_string(),
            version: server.inc_and_get_version_number(),
            text: file_content,
        },
    })
}

unsafe fn build_text_document_did_change(
    env: *mut emacs_env,
    request_args: emacs_value,
    server: &mut Server,
) -> NotificationParams {
    let file_path = nth(env, 0, request_args);
    let file_path = check_path(extract_string(env, file_path));
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
                version: server.inc_and_get_version_number(),
            },
            content_changes: json_content_changes,
        },
    )
}

unsafe fn build_text_document_did_close(
    env: *mut emacs_env,
    request_args: emacs_value,
    _server: &mut Server,
) -> NotificationParams {
    let file_path = nth(env, 0, request_args);
    let file_path = check_path(extract_string(env, file_path));
    let uri = file_path_to_uri(file_path);

    NotificationParams::DidCloseTextDocumentParams(DidCloseTextDocumentParams {
        text_document: TextDocumentIdentifier { uri },
    })
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_recv_response(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    log_args(env, nargs, args, "tlc__rust_recv_response");

    handle_call(env, nargs, args, |env, _args, server| {
        if let Some(recv_result) = server.try_recv_response() {
            let result = match recv_result {
                Some(response) => handle_response(env, response),
                None => intern(env, "no-response"),
            };
            Some(result)
        } else {
            None
        }
    })
}

unsafe fn handle_response(
    env: *mut emacs_env,
    response: Response,
) -> emacs_value {
    let id = make_integer(env, response.id as i64);
    if let Some(result) = response.result {
        if let Result::TextDocumentDefinitionResult(definition_result) = result
        {
            let location_list = match definition_result {
                DefinitionResult::LocationList(location_list) => location_list,
                DefinitionResult::LocationLinks(location_link_list) => {
                    location_link_list
                        .into_iter()
                        .map(|location_link| location_link.to_location())
                        .collect()
                }
            };
            let mut lisp_location_list_vec = Vec::new();

            for location in location_list {
                let uri = &location.uri;
                let range = &location.range;

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
            let lisp_location_list = call(env, "list", lisp_location_list_vec);
            call(
                env,
                "list",
                vec![intern(env, "response"), id, make_bool(env, true), lisp_location_list],
            )
        } else if let Result::TextDocumentCompletionResult(completion_result) =
            result
        {
            let mut completion_list_vec = Vec::new();

            let items = match completion_result {
                CompletionResult::CompletionList(CompletionList { items }) => {
                    items
                }
                CompletionResult::CompletionItems(items) => items,
            };

            for item in items {
                completion_list_vec
                    .push(make_string(env, str::trim_start(item.get_text())));
            }

            let completion_list = call(env, "list", completion_list_vec);

            // todo: find a way to avoid duplicating the non-null OK responses
            call(
                env,
                "list",
                vec![intern(env, "response"), id, make_bool(env, true), completion_list],
            )
        } else {
            logger::log_rust_debug!(
                "Non-supported response received: {:?}",
                result
            );
            intern(env, "error-response")
        }
    } else {
        if response.error.is_some() {
            intern(env, "error-response")
        } else {
            // Happens e.g. when rust-analyzer doesn't send any completion result
            call(env, "list", vec![intern(env, "response"), id, make_bool(env, false)])
        }
    }
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_set_option(
    env: *mut emacs_env,
    _nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    // Don't log args so that log file can change location before
    // any logging takes place
    let symbol = *args.offset(0);
    let symbol = extract_string(env, call(env, "symbol-name", vec![symbol]));
    let value = *args.offset(1);

    if symbol == "tlc-log-file" {
        let path = extract_string(env, value);
        logger::set_log_file_name(check_path(path));
    } else {
        let value = extract_bool(env, value);
        if symbol == "tlc-log-io" {
            logger::LOG_IO.store(value, Ordering::Relaxed)
        } else if symbol == "tlc-log-stderr" {
            logger::LOG_STDERR.store(value, Ordering::Relaxed)
        } else if symbol == "tlc-log-rust-debug" {
            logger::LOG_RUST_DEBUG.store(value, Ordering::Relaxed)
        } else if symbol == "tlc-log-to-stdio" {
            logger::LOG_TO_STDIO.store(value, Ordering::Relaxed)
        } else if symbol == "tlc-stop-server-on-stderr" {
            server::STOP_SERVER_ON_STDERR.store(value, Ordering::Relaxed)
        } else {
            panic!("Incorrect log symbol")
        };
    }

    intern(env, "nil")
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_log_emacs_debug(
    env: *mut emacs_env,
    _nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    let msg = *args.offset(0);
    let msg = extract_string(env, msg);
    logger::log_emacs_debug!("{}", msg);

    intern(env, "nil")
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_stop_server(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    log_args(env, nargs, args, "tlc__rust_stop_server");
    handle_call(env, nargs, args, |env, _args, server| {
        server.stop_server();
        Some(intern(env, "ok"))
    })
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

unsafe fn log_args<S: AsRef<str>>(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    function_name: S,
) {
    // logger::log_rust_debug! already knows whether to log or not. But check
    // anyway as an optimization so that lots of string and terms aren't
    // created unecessarily.
    if logger::is_log_enabled!(LOG_RUST_DEBUG) {
        let args_list = args_pointer_to_args_vec(nargs, args);
        let list = call(env, "list", args_list);
        let format_string = make_string(
            env,
            format!("{} arguments ({}) : %S", function_name.as_ref(), nargs),
        );
        let formatted = call(env, "format", vec![format_string, list]);
        let formatted = extract_string(env, formatted);
        logger::log_rust_debug!("{}", formatted);
    }
}

unsafe fn handle_call<
    F: FnOnce(
        *mut emacs_env,
        Vec<emacs_value>,
        &mut Server,
    ) -> Option<emacs_value>,
>(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    f: F,
) -> emacs_value {
    let args_vec = args_pointer_to_args_vec(nargs, args);

    let root_path = check_path(extract_string(env, args_vec[0]));
    servers::with_servers(|servers| {
        if let Some(ref mut server) = &mut servers.get_mut(&root_path) {
            if let Some(result) = f(env, args_vec, server) {
                result
            } else {
                // This means it failed during handling this call
                servers.remove(&root_path);
                intern(env, "no-server")
            }
        } else {
            // This means the server wasn't existing before this call
            intern(env, "no-server")
        }
    })
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
