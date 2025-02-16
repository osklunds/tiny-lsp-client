use super::*;
use std::fs;

#[test]
fn initialize() {
    logger::set_log_file_name("/tmp/tiny-lsp-client.log");

    let mut connection = Connection::new(
        "rust-analyzer",
        "/home/oskar/own_repos/tiny-lsp-client",
    )
    .unwrap();

    connection.initialize();
}

#[test]
fn did_open_change_close_and_definition() {
    logger::set_log_file_name("/tmp/tiny-lsp-client.log");

    let mut connection = Connection::new(
        "rust-analyzer",
        "/home/oskar/own_repos/tiny-lsp-client",
    )
    .unwrap();
    connection.initialize();

    let uri =
        "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs".to_string();

    // textDocument/didOpen
    connection.send_notification(
        "textDocument/didOpen".to_string(),
        NotificationParams::DidOpenTextDocumentParams(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: LANGUAGE_ID.to_string(),
                    version: 0,
                    text: fs::read_to_string(
                        "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs",
                    )
                    .unwrap(),
                },
            },
        ),
    );

    // textDocument/definition
    assert_eq!(Some(None), connection.try_recv_response());
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
            &mut connection,
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
    connection.send_notification(
        "textDocument/didChange".to_string(),
        NotificationParams::DidChangeTextDocumentParams(
            DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 3,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
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
                }],
            },
        ),
    );

    // textDocument/definition after textDocument/didChange
    let id = connection
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
    let response = connection.recv_response().unwrap();
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
    let id = connection
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
    let response = connection.recv_response().unwrap();
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
    connection.send_notification(
        "textDocument/didChange".to_string(),
        NotificationParams::DidChangeTextDocumentParams(
            DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version: 4,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
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
                }],
            },
        ),
    );

    // textDocument/didClose
    connection.send_notification(
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
    connection.send_notification(
        "textDocument/didOpen".to_string(),
        NotificationParams::DidOpenTextDocumentParams(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: LANGUAGE_ID.to_string(),
                    version: 0,
                    text: fs::read_to_string(
                        "/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs",
                    )
                    .unwrap(),
                },
            },
        ),
    );

    // textDocument/definition
    let id = connection
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
    let response = connection.recv_response().unwrap();
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
    connection: &mut Connection,
    current_id: u32,
    request_params: DefinitionParams,
) -> (Response, u32) {
    receive_until_definition_response_with_one_location_1(
        connection,
        current_id,
        request_params,
        1000,
    )
}

fn receive_until_definition_response_with_one_location_1(
    connection: &mut Connection,
    current_id: u32,
    request_params: DefinitionParams,
    retries: usize,
) -> (Response, u32) {
    let id = connection
        .send_request(
            "textDocument/definition".to_string(),
            RequestParams::DefinitionParams(request_params.clone()),
        )
        .unwrap();
    assert_eq!(current_id, id);
    let next_id = current_id + 1;
    let response = connection.recv_response().unwrap();
    if let Some(result) = &response.result {
        if let Result::TextDocumentDefinitionResult(result) = result {
            if let DefinitionResult::LocationLinkList(result) = result {
                if !result.is_empty() {
                    return (response, next_id);
                }
            } else {
                panic!("Not location_link as response");
            }
        }
    }
    if retries > 0 {
        thread::sleep(Duration::from_millis(100));
        receive_until_definition_response_with_one_location_1(
            connection,
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
    if let DefinitionResult::LocationLinkList(result) = result {
        assert_eq!(1, result.len());
        let location_link = result[0].clone();
        let target_range = &location_link.target_range;
        assert_eq!(&exp_target_range, target_range);
    } else {
        panic!("Not location_link as response");
    }
}
