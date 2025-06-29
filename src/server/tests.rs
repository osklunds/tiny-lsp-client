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

use super::*;
use std::env;
use std::fs;

#[test]
fn initialize() {
    logger::set_log_file_name("/tmp/tiny-lsp-client.log");

    let mut server = Server::new(
        &env::current_dir().unwrap().display().to_string(),
        "rust-analyzer",
    )
    .unwrap();

    server.initialize();
}

#[test]
fn did_open_change_close_and_definition() {
    logger::set_log_file_name("/tmp/tiny-lsp-client.log");

    let cwd = env::current_dir().unwrap().display().to_string();

    let mut server = Server::new(&cwd, "rust-analyzer").unwrap();
    server.initialize();

    // todo: skip using dummy.rs, use the one from test/rust_analyzer instead
    let dummy_file_path = format!("{}/src/dummy.rs", cwd);
    let uri = format!("file://{}", dummy_file_path);

    // textDocument/didOpen
    server.send_notification(
        "textDocument/didOpen".to_string(),
        NotificationParams::DidOpenTextDocumentParams(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: LANGUAGE_ID.to_string(),
                    version: 0,
                    text: fs::read_to_string(&dummy_file_path).unwrap(),
                },
            },
        ),
    );

    // textDocument/definition
    // Use both with and without timeout to increase test coverage
    assert_eq!(Some(None), server.try_recv_response(None));
    assert_eq!(
        Some(None),
        server.try_recv_response(Some(Duration::from_millis(100)))
    );
    let request_params = DefinitionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        position: Position {
            character: 4,
            line: 4,
        },
    };
    // Need to loop until response because rust-analyzer takes time to start
    // and sends different responses until it's ready
    let (response, base_id) =
        request_definition_until_response_with_one_location(
            &mut server,
            1,
            request_params,
        );
    assert_definition_response(
        Range {
            start: Position {
                line: 7,
                character: 0,
            },
            end: Position {
                line: 9,
                character: 1,
            },
        },
        response,
    );

    // textDocument/didChange
    server.send_notification(
        "textDocument/didChange".to_string(),
        NotificationParams::DidChangeTextDocumentParams(
            DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 3,
                },
                content_changes: vec![
                    TextDocumentContentChangeEventIncremental {
                        range: Range {
                            start: Position {
                                line: 6,
                                character: 0,
                            },
                            end: Position {
                                line: 6,
                                character: 0,
                            },
                        },
                        text: "\n".to_string(),
                    },
                ],
            },
        ),
    );

    // textDocument/definition after textDocument/didChange
    let id = server
        .send_request(
            "textDocument/definition".to_string(),
            RequestParams::DefinitionParams(DefinitionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    character: 4,
                    line: 4,
                },
            }),
        )
        .unwrap();
    assert_eq!(base_id, id);
    let response = server.recv_response().unwrap();
    assert_definition_response(
        Range {
            start: Position {
                line: 8,
                character: 0,
            },
            end: Position {
                line: 10,
                character: 1,
            },
        },
        response,
    );

    // textDocument/definition after textDocument/didChange again
    let id = server
        .send_request(
            "textDocument/definition".to_string(),
            RequestParams::DefinitionParams(DefinitionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    character: 4,
                    line: 4,
                },
            }),
        )
        .unwrap();
    assert_eq!(base_id + 1, id);
    let response = server.recv_response().unwrap();
    assert_definition_response(
        Range {
            start: Position {
                line: 8,
                character: 0,
            },
            end: Position {
                line: 10,
                character: 1,
            },
        },
        response,
    );

    // textDocument/didChange to revert the previous change, so that
    // rust-analyzer's view matches the file system
    server.send_notification(
        "textDocument/didChange".to_string(),
        NotificationParams::DidChangeTextDocumentParams(
            DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 4,
                },
                content_changes: vec![
                    TextDocumentContentChangeEventIncremental {
                        range: Range {
                            start: Position {
                                line: 6,
                                character: 0,
                            },
                            end: Position {
                                line: 7,
                                character: 1,
                            },
                        },
                        text: "".to_string(),
                    },
                ],
            },
        ),
    );

    // textDocument/didClose
    server.send_notification(
        "textDocument/didClose".to_string(),
        NotificationParams::DidCloseTextDocumentParams(
            DidCloseTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
            },
        ),
    );

    // Do a definition again to ensure that rust-analyzer did not crash due to
    // faulty data sent in didClose above. Since it's a notification we can't
    // wait for a response.

    // textDocument/didOpen
    server.send_notification(
        "textDocument/didOpen".to_string(),
        NotificationParams::DidOpenTextDocumentParams(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: LANGUAGE_ID.to_string(),
                    version: 0,
                    text: fs::read_to_string(&dummy_file_path).unwrap(),
                },
            },
        ),
    );

    // textDocument/definition
    let id = server
        .send_request(
            "textDocument/definition".to_string(),
            RequestParams::DefinitionParams(DefinitionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    character: 4,
                    line: 4,
                },
            }),
        )
        .unwrap();
    assert_eq!(base_id + 2, id);
    let response = server.recv_response().unwrap();
    assert_definition_response(
        Range {
            start: Position {
                line: 7,
                character: 0,
            },
            end: Position {
                line: 9,
                character: 1,
            },
        },
        response,
    );
}

fn request_definition_until_response_with_one_location(
    server: &mut Server,
    current_id: u32,
    request_params: DefinitionParams,
) -> (Response, u32) {
    receive_until_definition_response_with_one_location_1(
        server,
        current_id,
        request_params,
        1000,
    )
}

fn receive_until_definition_response_with_one_location_1(
    server: &mut Server,
    current_id: u32,
    request_params: DefinitionParams,
    retries: usize,
) -> (Response, u32) {
    let id = server
        .send_request(
            "textDocument/definition".to_string(),
            RequestParams::DefinitionParams(request_params.clone()),
        )
        .unwrap();
    assert_eq!(current_id, id);
    let next_id = current_id + 1;
    let response = server.recv_response().unwrap();
    if let Some(result) = &response.result {
        if let Result::TextDocumentDefinitionResult(result) = result {
            if let DefinitionResult::LocationLinks(result) = result {
                if !result.is_empty() {
                    return (response, next_id);
                }
            }
        }
    }
    if retries > 0 {
        thread::sleep(Duration::from_millis(100));
        receive_until_definition_response_with_one_location_1(
            server,
            next_id,
            request_params,
            retries - 1,
        )
    } else {
        panic!()
    }
}

fn assert_definition_response(exp_target_range: Range, response: Response) {
    let result = response.result.unwrap();
    let Result::TextDocumentDefinitionResult(result) = result else {
        panic!()
    };
    if let DefinitionResult::LocationLinks(result) = result {
        assert_eq!(1, result.len());
        let location_link = result[0].clone();
        let target_range = &location_link.target_range;
        assert_eq!(&exp_target_range, target_range);
    } else {
        panic!("Not location_link as response");
    }
}
