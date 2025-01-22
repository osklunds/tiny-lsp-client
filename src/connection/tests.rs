
use super::*;
use std::fs;

#[test]
fn initialize() {
    let mut connection = Connection::new(
        "rust-analyzer",
        "/home/oskar/own_repos/tiny-lsp-client"
    );

    connection.initialize();
}

#[test]
fn did_open_change_close_and_definition() {
    let mut connection = Connection::new(
        "rust-analyzer",
        "/home/oskar/own_repos/tiny-lsp-client"
    );
    connection.initialize();

    // let rust-analyzer have time to start
    // todo: consider looping again
    thread::sleep(Duration::from_millis(2000));

    let uri = "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs".to_string();

    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "rust".to_string(),
            version: 0,
            text: fs::read_to_string("/home/oskar/own_repos/tiny-lsp-client/src/dummy.rs").unwrap()
        }
    };
    let did_open_params = NotificationParams::DidOpenTextDocumentParams(did_open_params);
    let did_open = Notification {
        method: "textDocument/didOpen".to_string(),
        params: did_open_params
    };
    let did_open = Message::Notification(did_open);
    connection.send_msg(did_open);

    let definition_params = DefinitionParams {
        text_document: TextDocumentIdentifier {
            uri: uri.clone()
        },
        position: Position {
            character: 4,
            line: 4
        }
    };
    let definition_params = RequestParams::DefinitionParams(definition_params);
    let definition = Request {
        id: 1234,
        method: "textDocument/definition".to_string(),
        params: definition_params
    };
    let definition = Message::Request(definition);
    connection.send_msg(definition.clone());
    let response = connection.recv_response();
    let result = response.result.unwrap();
    let Result::TextDocumentDefinitionResult(result) = result else { panic!() };
    let DefinitionResult::LocationLinkList(result) = result else { panic!() };
    assert_eq!(1, result.len());
    let location_link = result[0].clone();
    let target_range = &location_link.target_range;
    let exp_target_range = Range {
        start: Position {
            line: 7,
            character: 0
        },
        end: Position {
            line: 9,
            character: 1
        }
    };
    assert_eq!(&exp_target_range, target_range);

    let did_change_params = DidChangeTextDocumentParams {
        text_document: VersionedTextDocumentIdentifier {
            uri: uri.clone(),
            version: 3
        },
        content_changes: vec![TextDocumentContentChangeEvent {
            range: Range {
                start: Position {
                    line: 6,
                    character: 0
                },
                end: Position {
                    line: 6,
                    character: 0
                }
            },
            text: "\n".to_string()
        }
        ]
    };
    let did_change_params =
        NotificationParams::DidChangeTextDocumentParams(did_change_params);
    let did_change = Notification {
        method: "textDocument/didChange".to_string(),
        params: did_change_params
    };
    let did_change = Message::Notification(did_change);
    connection.send_msg(did_change);
    thread::sleep(Duration::from_millis(1000));

    // Ask for same position
    let definition_params = DefinitionParams {
        text_document: TextDocumentIdentifier {
            uri: uri.clone()
        },
        position: Position {
            character: 4,
            line: 4
        }
    };
    let definition_params = RequestParams::DefinitionParams(definition_params);
    let definition = Request {
        id: 1235,
        method: "textDocument/definition".to_string(),
        params: definition_params
    };
    let definition = Message::Request(definition);
    connection.send_msg(definition);
    let response = connection.recv_response();
    let result = response.result.unwrap();
    let Result::TextDocumentDefinitionResult(result) = result else { panic!() };
    let DefinitionResult::LocationLinkList(result) = result else { panic!() };
    assert_eq!(1, result.len());
    let location_link = result[0].clone();
    let target_range = &location_link.target_range;
    // Due to change, different position
    let exp_target_range = Range {
        start: Position {
            line: 8,
            character: 0
        },
        end: Position {
            line: 10,
            character: 1
        }
    };
    assert_eq!(&exp_target_range, target_range);
}
