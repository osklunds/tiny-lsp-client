use super::*;
use std::fs;

#[test]
fn initialize() {
    let mut connection = Connection::new(
        "rust-analyzer",
        "/home/oskar/own_repos/tiny-lsp-client",
    );

    connection.initialize();
}

#[test]
fn did_open_change_close_and_definition() {
    let mut connection = Connection::new(
        "rust-analyzer",
        "/home/oskar/own_repos/tiny-lsp-client",
    );
    connection.initialize();

    // let rust-analyzer have time to start
    // todo: consider looping again
    thread::sleep(Duration::from_millis(2000));

    let uri =
        "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs".to_string();

    // textDocument/didOpen
    connection.send_notification(
        "textDocument/didOpen".to_string(),
        NotificationParams::DidOpenTextDocumentParams(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "rust".to_string(),
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
    assert_eq!(None, connection.try_recv_response());
    connection.send_request(
        "textDocument/definition".to_string(),
        RequestParams::DefinitionParams(DefinitionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                character: 4,
                line: 4,
            },
        }),
    );
    let response = connection.recv_response();
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
    connection.send_request(
        "textDocument/definition".to_string(),
        RequestParams::DefinitionParams(DefinitionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                character: 4,
                line: 4,
            },
        }),
    );
    let response = connection.recv_response();
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
    connection.send_request(
        "textDocument/definition".to_string(),
        RequestParams::DefinitionParams(DefinitionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                character: 4,
                line: 4,
            },
        }),
    );
    let response = connection.recv_response();
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
                    language_id: "rust".to_string(),
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
    connection.send_request(
        "textDocument/definition".to_string(),
        RequestParams::DefinitionParams(DefinitionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                character: 4,
                line: 4,
            },
        }),
    );
    let response = connection.recv_response();
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

fn assert_definition_response(exp_target_range: Range, response: Response) {
    let result = response.result.unwrap();
    let Result::TextDocumentDefinitionResult(result) = result else {
        panic!()
    };
    let DefinitionResult::LocationLinkList(result) = result else {
        panic!()
    };
    assert_eq!(1, result.len());
    let location_link = result[0].clone();
    let target_range = &location_link.target_range;
    assert_eq!(&exp_target_range, target_range);
}
