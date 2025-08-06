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

// @credits: This module as a whole is inspired by
// https://github.com/zbelial/lspce and
// https://ryanfaulhaber.com/posts/first-emacs-module-rust/

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
use crate::servers::ServerKey;

use std::os::raw;
use std::str;
use std::sync::atomic::Ordering;
use std::time::Duration;

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
        1,
        1,
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
        2,
        2,
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

    servers::with_servers(|servers| {
        handle_none(env, || {
            let mut server_info_list = Vec::new();
            let mut servers: Vec<_> = servers.iter().collect();
            servers.sort_by_key(|(&ref server_key, _server)| server_key);

            for (server_key, server) in servers.iter() {
                let info = (
                    &server_key.root_path,
                    &server_key.server_cmd,
                    server.get_server_process_id() as i64,
                );
                server_info_list.push(info);
            }
            server_info_list
        })
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

    servers::with_servers(|servers| {
        handle_none(env, || {
            let args_vec = args_pointer_to_args_vec(nargs, args);
            let server_key = get_server_key(env, &args_vec);
            if servers.contains_key(&server_key) {
                return RustCallResult::<i64>::Symbol("already-started");
            } else {
                logger::log_rust_debug!("Need to start new");
            }
            match Server::new(&server_key.root_path, &server_key.server_cmd) {
                Some(mut server) => match server.initialize() {
                    Some(()) => {
                        servers.insert(server_key, server);
                        RustCallResult::Symbol("started")
                    }
                    None => RustCallResult::Symbol("start-failed"),
                },
                None => RustCallResult::Symbol("start-failed"),
            }
        })
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

        let request_params = match request_type.as_str() {
            "textDocument/definition" => {
                build_text_document_definition(env, request_args, server)
            }
            "textDocument/completion" => {
                build_text_document_completion(env, request_args, server)
            }
            "textDocument/hover" => {
                build_text_document_hover(env, request_args, server)
            }
            _ => {
                panic!("Incorrect request type")
            }
        };
        server.send_request(request_type, request_params)
    })
}

#[allow(non_snake_case)]
unsafe fn build_text_document_definition(
    env: *mut emacs_env,
    request_args: emacs_value,
    _server: &mut Server,
) -> RequestParams {
    let uri = nth(env, 0, request_args);
    let uri = extract_string(env, uri);

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
    let uri = nth(env, 0, request_args);
    let uri = extract_string(env, uri);

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
unsafe fn build_text_document_hover(
    env: *mut emacs_env,
    request_args: emacs_value,
    _server: &mut Server,
) -> RequestParams {
    let uri = nth(env, 0, request_args);
    let uri = extract_string(env, uri);

    let line = nth(env, 1, request_args);
    let line = extract_integer(env, line) as usize;

    let character = nth(env, 2, request_args);
    let character = extract_integer(env, character) as usize;

    RequestParams::HoverParams(HoverParams {
        text_document: TextDocumentIdentifier { uri },
        position: Position { line, character },
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
            .map(|_| RustCallResult::<u32>::Symbol("ok"))
    })
}

unsafe fn build_text_document_did_open(
    env: *mut emacs_env,
    request_args: emacs_value,
    server: &mut Server,
) -> NotificationParams {
    let uri = nth(env, 0, request_args);
    let uri = extract_string(env, uri);

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
    let uri = nth(env, 0, request_args);
    let uri = extract_string(env, uri);

    let content_changes = nth(env, 1, request_args);
    let content_changes_len = call(env, "length", vec![content_changes]);
    let content_changes_len = extract_integer(env, content_changes_len);

    let mut json_content_changes = Vec::new();

    for i in 0..content_changes_len {
        let content_change = nth(env, i, content_changes);

        let text = extract_string(env, nth(env, 0, content_change));
        let content_change_len =
            extract_integer(env, call(env, "length", vec![content_change]));

        // len 1 means full change, so the other elements don't exist
        let json_content_change = if content_change_len == 1 {
            TextDocumentContentChangeEvent::TextDocumentContentChangeEventFull(
                TextDocumentContentChangeEventFull { text },
            )
        } else {
            let start_line = nth(env, 1, content_change);
            let start_character = nth(env, 2, content_change);
            let end_line = nth(env, 3, content_change);
            let end_character = nth(env, 4, content_change);

            TextDocumentContentChangeEvent::TextDocumentContentChangeEventIncremental(
                TextDocumentContentChangeEventIncremental {
                    range: Range {
                        start: Position {
                            line: extract_integer(env, start_line) as usize,
                            character: extract_integer(env, start_character)
                                as usize,
                        },
                        end: Position {
                            line: extract_integer(env, end_line) as usize,
                            character: extract_integer(env, end_character)
                                as usize,
                        },
                    },
                    text,
                },
            )
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
    let uri = nth(env, 0, request_args);
    let uri = extract_string(env, uri);

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

    handle_call(env, nargs, args, |env, args, server| {
        let timeout = extract_integer(env, args[1]);
        let timeout = if timeout == 0 {
            None
        } else {
            Some(Duration::from_millis(timeout as u64))
        };
        if let Some(recv_result) = server.try_recv_response(timeout) {
            let result = match recv_result {
                Some(response) => {
                    RustCallResult::Any(handle_response::<u32>(response))
                }
                None => RustCallResult::Symbol("no-response"),
            };
            Some(result)
        } else {
            None
        }
    })
}

unsafe fn handle_response<A: IntoLisp>(
    response: Response,
) -> RustCallResult<(RustCallResult<A>, u32, bool, HandleResponse)> {
    if let Some(result) = response.result {
        if let Result::Untyped(_) = result {
            logger::log_rust_debug!(
                "Non-supported response received: {:?}",
                result
            );
            RustCallResult::Symbol("error-response")
        } else {
            let return_value = match result {
                Result::TextDocumentDefinitionResult(definition_result) => {
                    HandleResponse::DefinitionResponse(
                        handle_definition_response(definition_result),
                    )
                }
                Result::TextDocumentCompletionResult(completion_result) => {
                    HandleResponse::CompletionResponse(
                        handle_completion_response(completion_result),
                    )
                }
                Result::TextDocumentHoverResult(hover_result) => {
                    HandleResponse::HoverResponse(handle_hover_response(
                        hover_result,
                    ))
                }
                _ => panic!("case already handled"),
            };
            RustCallResult::Any((
                RustCallResult::<A>::Symbol("response"),
                response.id,
                true,
                return_value,
            ))
        }
    } else {
        if response.error.is_some() {
            RustCallResult::Symbol("error-response")
        } else {
            // Happens e.g. when rust-analyzer doesn't send any completion result
            RustCallResult::Any((
                RustCallResult::Symbol("response"),
                response.id,
                false,
                HandleResponse::NullResponse,
            ))
        }
    }
}

unsafe fn handle_definition_response(
    response: DefinitionResult,
) -> Vec<(String, usize, usize)> {
    let location_list = match response {
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

        // todo: don't clone
        let lisp_location =
            (uri.clone(), range.start.line, range.start.character);
        lisp_location_list_vec.push(lisp_location);
    }
    lisp_location_list_vec
}

unsafe fn handle_completion_response(
    response: CompletionResult,
) -> Vec<String> {
    let mut completion_list_vec = Vec::new();

    let items = match response {
        CompletionResult::CompletionList(CompletionList { items }) => items,
        CompletionResult::CompletionItems(items) => items,
    };

    for item in items {
        // todo: don't clone
        completion_list_vec.push(str::trim_start(item.get_text()).to_string());
    }

    completion_list_vec
}

unsafe fn handle_hover_response(response: HoverResult) -> String {
    response.contents.value
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
        logger::set_log_file_name(path);
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
    handle_call(env, nargs, args, |_env, _args, server| {
        // could consider removing from servers, but in case the stopping fails
        // it's good that the server remains in the list
        server.stop_server();
        Some(RustCallResult::<u32>::Symbol("ok"))
    })
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
    // Idea: pass lambda that is lazily called. So can do a general
    // optimization without macros
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
    T: IntoLisp,
    F: Copy + FnOnce(*mut emacs_env, Vec<emacs_value>, &mut Server) -> Option<T>,
>(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    f: F,
) -> emacs_value {
    servers::with_servers(|servers| {
        handle_none(env, || {
            let args_vec = args_pointer_to_args_vec(nargs, args);
            let server_key = get_server_key(env, &args_vec);
            if let Some(ref mut server) = &mut servers.get_mut(&server_key) {
                if let Some(result) = f(env, args_vec, server) {
                    RustCallResult::Any(result)
                } else {
                    // This means it failed during handling this call
                    servers.remove(&server_key);
                    RustCallResult::Symbol("no-server")
                }
            } else {
                // This means the server wasn't existing before this call
                RustCallResult::Symbol("no-server")
            }
        })
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

unsafe fn get_server_key(
    env: *mut emacs_env,
    args: &[emacs_value],
) -> ServerKey {
    let server_key = args[0];
    // Asserts because if not fulfilled, endless loop is entered, which is
    // harder to debug. The loop happens due to non-local exit handling in
    // emacs.rs.
    // Todo: Could consider to have limited retries in that loop.
    assert!(extract_bool(env, call(env, "listp", vec![server_key])));
    assert_eq!(
        extract_integer(env, call(env, "length", vec![server_key])),
        2
    );
    let root_path = extract_string(env, nth(env, 0, server_key));
    let server_cmd = extract_string(env, nth(env, 1, server_key));
    ServerKey {
        root_path,
        server_cmd,
    }
}

enum RustCallResult<A: IntoLisp> {
    Symbol(&'static str),
    Any(A),
}

impl<A: IntoLisp> IntoLisp for RustCallResult<A> {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        match self {
            Self::Symbol(string) => intern_new(env, string),
            Self::Any(value) => value.into_lisp(env),
        }
    }
}

enum HandleResponse {
    DefinitionResponse(Vec<(String, usize, usize)>),
    CompletionResponse(Vec<String>),
    HoverResponse(String),
    NullResponse,
}

impl IntoLisp for HandleResponse {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> Option<emacs_value> {
        match self {
            Self::DefinitionResponse(a) => a.into_lisp(env),
            Self::CompletionResponse(a) => a.into_lisp(env),
            Self::HoverResponse(a) => a.into_lisp(env),
            Self::NullResponse => false.into_lisp(env),
        }
    }
}
