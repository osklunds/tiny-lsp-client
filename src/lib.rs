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

    // todo: test in lisp-bindings-test
    export_function(
        env,
        1,
        1,
        tlc__rust_log_emacs_debug,
        "tlc--rust-log-emacs-debug",
    );

    export_function(env, 1, 1, tlc__rust_stop_server, "tlc--rust-stop-server");

    // todo: emit warning if don't unwrap
    // todo: own env wrapper that returns None if already an error
    // todo: Improve RustCallResult. Maybe symbol should be something
    // separate.
    provide_tlc_rust(env);

    0
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_all_server_info(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    lisp_function_in_rust(env, nargs, args, "tlc__rust_all_server_info", |()| {
        servers::with_servers(|servers| {
            let mut server_info_list = Vec::new();
            let mut servers: Vec<_> = servers.iter().collect();
            servers.sort_by_key(|(&ref server_key, _server)| server_key);

            for (server_key, server) in servers.iter() {
                let info = (
                    // The cloning can be avoided by having
                    // servers::with_servers come before
                    // lisp_function_in_rust. But tlc__rust_all_server_info is
                    // not on the critical path
                    server_key.root_path.clone(),
                    server_key.server_cmd.clone(),
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
    lisp_function_in_rust(
        env,
        nargs,
        args,
        "tlc__rust_start_server",
        |(server_key,)| {
            servers::with_servers(|servers| {
                if servers.contains_key(&server_key) {
                    return symbol("already-started");
                } else {
                    logger::log_rust_debug!("Need to start new");
                }
                match Server::new(&server_key.root_path, &server_key.server_cmd)
                {
                    Some(mut server) => match server.initialize() {
                        Some(()) => {
                            servers.insert(server_key, server);
                            symbol("started")
                        }
                        None => symbol("start-failed"),
                    },
                    None => symbol("start-failed"),
                }
            })
        },
    )
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_send_request(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    lisp_function_in_rust(
        env,
        nargs,
        args,
        "tlc__rust_send_request",
        |(server_key, method, request_args): (ServerKey, String, (_, _, _))| {
            // todo: By accident, request_args are the same for all requests
            // when they no longer are, need to have an enum of the variants
            let (uri, line, character) = request_args;
            handle_call(server_key, |server| {
                let request_params = match method.as_str() {
                    "textDocument/definition" => {
                        build_text_document_definition(uri, line, character)
                    }
                    "textDocument/completion" => {
                        build_text_document_completion(uri, line, character)
                    }
                    "textDocument/hover" => {
                        build_text_document_hover(uri, line, character)
                    }
                    _ => {
                        // todo: emacs error instead
                        panic!("Incorrect method")
                    }
                };
                server.send_request(method, request_params)
            })
        },
    )
}

fn build_text_document_definition(
    uri: String,
    line: i64,
    character: i64,
) -> RequestParams {
    RequestParams::DefinitionParams(DefinitionParams {
        text_document: TextDocumentIdentifier { uri },
        position: Position {
            line: line as usize,
            character: character as usize,
        },
    })
}

#[allow(non_snake_case)]
unsafe fn build_text_document_completion(
    uri: String,
    line: i64,
    character: i64,
) -> RequestParams {
    RequestParams::CompletionParams(CompletionParams {
        text_document: TextDocumentIdentifier { uri },
        position: Position {
            line: line as usize,
            character: character as usize,
        },
        context: CompletionContext { trigger_kind: 1 },
    })
}

#[allow(non_snake_case)]
unsafe fn build_text_document_hover(
    uri: String,
    line: i64,
    character: i64,
) -> RequestParams {
    RequestParams::HoverParams(HoverParams {
        text_document: TextDocumentIdentifier { uri },
        position: Position {
            line: line as usize,
            character: character as usize,
        },
    })
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_send_notification(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    lisp_function_in_rust(
        env,
        nargs,
        args,
        "tlc__rust_send_notification",
        |(server_key, method, request_args)| {
            handle_call(server_key, |server| {
                let notification_params = if method == "textDocument/didOpen" {
                    // todo: the ? here are incorrect, because it leads
                    // to no-server. It should be lisp error
                    let (uri, file_content) = match request_args {
                        SendNotificationParameters::UriFileContent(
                            uri,
                            file_content,
                        ) => Some((uri, file_content)),
                        _ => None,
                    }?;
                    build_text_document_did_open(uri, file_content, server)
                } else if method == "textDocument/didChange" {
                    let (uri, content_changes) = match request_args {
                        SendNotificationParameters::UriContentChanges(
                            uri,
                            content_changes,
                        ) => Some((uri, content_changes)),
                        _ => None,
                    }?;
                    build_text_document_did_change(uri, content_changes, server)
                } else if method == "textDocument/didClose" {
                    // todo: lisp error if None
                    let uri = match request_args {
                        SendNotificationParameters::Uri(uri) => Some(uri),
                        _ => None,
                    }?;
                    build_text_document_did_close(uri)
                } else {
                    panic!("Incorrect request type")
                };
                server
                    .send_notification(method, notification_params)
                    .map(|_| symbol("ok"))
            })
        },
    )
}

fn build_text_document_did_open(
    uri: String,
    file_content: String,
    server: &mut Server,
) -> NotificationParams {
    NotificationParams::DidOpenTextDocumentParams(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri,
            language_id: LANGUAGE_ID.to_string(),
            version: server.inc_and_get_version_number(),
            text: file_content,
        },
    })
}

fn build_text_document_did_change(
    uri: String,
    content_changes: Vec<(
        String,
        Option<usize>,
        Option<usize>,
        Option<usize>,
        Option<usize>,
    )>,
    server: &mut Server,
) -> NotificationParams {
    let mut json_content_changes = Vec::new();

    for content_change in content_changes {
        let (text, start_line, start_character, end_line, end_character) =
            content_change;

        let json_content_change = if start_line.is_none() {
            TextDocumentContentChangeEvent::TextDocumentContentChangeEventFull(
                TextDocumentContentChangeEventFull { text },
            )
        } else {
            TextDocumentContentChangeEvent::TextDocumentContentChangeEventIncremental(
                TextDocumentContentChangeEventIncremental {
                    range: Range {
                        start: Position {
                            line: start_line.unwrap(),
                            character: start_character.unwrap()
                        },
                        end: Position {
                            line: end_line.unwrap(),
                            character: end_character.unwrap()
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

fn build_text_document_did_close(uri: String) -> NotificationParams {
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
    lisp_function_in_rust(
        env,
        nargs,
        args,
        "tlc__rust_recv_response",
        |(server_key, timeout): (ServerKey, u64)| {
            handle_call(server_key, |server| {
                let timeout = if timeout == 0 {
                    None
                } else {
                    Some(Duration::from_millis(timeout))
                };
                if let Some(recv_result) = server.try_recv_response(timeout) {
                    let result = match recv_result {
                        Some(response) => RustCallResult::Any(
                            handle_response::<u32>(response),
                        ),
                        None => RustCallResult::Symbol("no-response"),
                    };
                    Some(result)
                } else {
                    None
                }
            })
        },
    )
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
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    // Don't log args so that log file can change location before
    // any logging takes place
    lisp_function_in_rust_no_args_log(
        env,
        nargs,
        args,
        |(symbol, value): (Symbol, SetOptionValue)| {
            if symbol.0 == "tlc-log-file" {
                let file_name = match value {
                    SetOptionValue::FileName(file_name) => file_name,
                    _ => todo!("raise lisp error"),
                };
                logger::set_log_file_name(file_name);
            } else {
                let value = match value {
                    SetOptionValue::Bool(value) => value,
                    _ => todo!("raise lisp error"),
                };
                if symbol.0 == "tlc-log-io" {
                    logger::LOG_IO.store(value, Ordering::Relaxed)
                } else if symbol.0 == "tlc-log-stderr" {
                    logger::LOG_STDERR.store(value, Ordering::Relaxed)
                } else if symbol.0 == "tlc-log-rust-debug" {
                    logger::LOG_RUST_DEBUG.store(value, Ordering::Relaxed)
                } else if symbol.0 == "tlc-log-to-stdio" {
                    logger::LOG_TO_STDIO.store(value, Ordering::Relaxed)
                } else if symbol.0 == "tlc-stop-server-on-stderr" {
                    server::STOP_SERVER_ON_STDERR
                        .store(value, Ordering::Relaxed)
                } else {
                    panic!("Incorrect log symbol")
                };
            };
        },
    )
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_log_emacs_debug(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    lisp_function_in_rust_no_args_log(env, nargs, args, |(msg,): (String,)| {
        logger::log_emacs_debug!("{}", msg);
    })
}

#[allow(non_snake_case)]
unsafe extern "C" fn tlc__rust_stop_server(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    _data: *mut raw::c_void,
) -> emacs_value {
    lisp_function_in_rust(
        env,
        nargs,
        args,
        "tlc__rust_stop_server",
        |(server_key,)| {
            handle_call(server_key, |server| {
                // could consider removing from servers, but in case the stopping fails
                // it's good that the server remains in the list
                server.stop_server();
                Some(symbol("ok"))
            })
        },
    )
}

fn handle_call<T: IntoLisp, F: FnOnce(&mut Server) -> Option<T>>(
    server_key: ServerKey,
    function: F,
) -> RustCallResult<T> {
    servers::with_servers(|servers| {
        if let Some(ref mut server) = &mut servers.get_mut(&server_key) {
            if let Some(result) = function(server) {
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
}

// todo: improve
enum RustCallResult<A: IntoLisp> {
    Symbol(&'static str),
    Any(A),
}

// Add derive, but perhaps these should be removed
impl<A: IntoLisp> IntoLisp for RustCallResult<A> {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        match self {
            Self::Symbol(string) => symbol(string).into_lisp(env),
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
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        match self {
            Self::DefinitionResponse(a) => a.into_lisp(env),
            Self::CompletionResponse(a) => a.into_lisp(env),
            Self::HoverResponse(a) => a.into_lisp(env),
            Self::NullResponse => false.into_lisp(env),
        }
    }
}

impl IntoLisp for ServerKey {
    unsafe fn into_lisp(self, env: *mut emacs_env) -> LispResult<emacs_value> {
        let ServerKey {
            root_path,
            server_cmd,
        } = self;
        (root_path, server_cmd).into_lisp(env)
    }
}

impl FromLisp for ServerKey {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<ServerKey> {
        let (root_path, server_cmd) = FromLisp::from_lisp(env, value)?;
        Ok(ServerKey {
            root_path,
            server_cmd,
        })
    }
}

enum SendNotificationParameters {
    Uri(String),
    UriFileContent(String, String),
    UriContentChanges(
        String,
        // todo: struct for ContentChange
        Vec<(
            String,
            Option<usize>,
            Option<usize>,
            Option<usize>,
            Option<usize>,
        )>,
    ),
}

impl FromLisp for SendNotificationParameters {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<SendNotificationParameters> {
        // An option would be to do from_lisp directly for each variant, and if
        // Some, then we're done. But there are some issues with that approach:
        // - Performance waste instead of trying and failing. Since this code
        //   is used for didChange, it is one the most critical path.
        // - If a conversion fails, it causes a non-local exit, which is ugly
        //   if it happens due to something which is not a bug
        if !call_lisp_rust(env, "listp", vec![value])? {
            Err(())
        } else {
            match call_lisp_rust(env, "length", vec![value])? {
                1 => {
                    let (uri,) = FromLisp::from_lisp(env, value)?;
                    Ok(SendNotificationParameters::Uri(uri))
                }
                2 => {
                    let first = call_lisp_lisp(env, "car", vec![value])?;
                    let second = call_lisp_lisp(env, "cadr", vec![value])?;
                    if call_lisp_rust(env, "listp", vec![second])? {
                        let uri = FromLisp::from_lisp(env, first)?;
                        let content_changes = FromLisp::from_lisp(env, second)?;
                        Ok(SendNotificationParameters::UriContentChanges(
                            uri,
                            content_changes,
                        ))
                    } else {
                        let uri = FromLisp::from_lisp(env, first)?;
                        let file_content = FromLisp::from_lisp(env, second)?;
                        Ok(SendNotificationParameters::UriFileContent(
                            uri,
                            file_content,
                        ))
                    }
                }
                _ => {
                    todo!("raise lisp error")
                }
            }
        }
    }
}

enum SetOptionValue {
    Bool(bool),
    FileName(String),
}

impl FromLisp for SetOptionValue {
    unsafe fn from_lisp(
        env: *mut emacs_env,
        value: emacs_value,
    ) -> LispResult<SetOptionValue> {
        if call_lisp_rust(env, "stringp", vec![value])? {
            Ok(Self::FileName(FromLisp::from_lisp(env, value)?))
        } else {
            Ok(Self::Bool(FromLisp::from_lisp(env, value)?))
        }
    }
}
