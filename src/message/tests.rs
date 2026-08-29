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

use serde_json::json;

#[test]
fn completion_item_label() {
    let json: serde_json::Value = json!({
        "jsonrpc": "2.0".to_string(),
        "id": 123,
        "result": {
            "items": [
                {
                    "label": "some_label",
                }
            ]
        }
    });

    let decoded = Message::Response(Response {
        jsonrpc: "2.0".to_string(),
        id: 123,
        result: Some(Result::TextDocumentCompletionResult(
            CompletionResult::CompletionList(CompletionList {
                items: vec![CompletionItem {
                    label: "some_label".to_string(),
                    insert_text: None,
                    text_edit: None,
                }],
            }),
        )),
        error: None,
    });

    assert_json_decodes_into(json, decoded);
}

fn assert_json_decodes_into(json: serde_json::Value, exp_decoded: Message) {
    let encoded: String = serde_json::to_string(&json).unwrap();
    let decoded: Message = serde_json::from_str(&encoded).unwrap();
    assert_eq!(exp_decoded, decoded);
}

#[test]
fn completion_item_label_and_insert_text() {
    let json: serde_json::Value = json!({
        "jsonrpc": "2.0".to_string(),
        "id": 123,
        "result": {
            "items": [
                {
                    "label": "some_label",
                    "insertText": "some_insert_text",
                }
            ]
        }
    });

    let decoded = Message::Response(Response {
        jsonrpc: "2.0".to_string(),
        id: 123,
        result: Some(Result::TextDocumentCompletionResult(
            CompletionResult::CompletionList(CompletionList {
                items: vec![CompletionItem {
                    label: "some_label".to_string(),
                    insert_text: Some("some_insert_text".to_string()),
                    text_edit: None,
                }],
            }),
        )),
        error: None,
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn completion_item_label_and_insert_text_and_text_edit() {
    let json: serde_json::Value = json!({
        "jsonrpc": "2.0".to_string(),
        "id": 123,
        "result": {
            "items": [
                {
                    "label": "some_label",
                    "insertText": "some_insert_text",
                    "textEdit": {
                        "newText": "some_new_text"
                    }
                }
            ]
        }
    });

    let decoded = Message::Response(Response {
        jsonrpc: "2.0".to_string(),
        id: 123,
        result: Some(Result::TextDocumentCompletionResult(
            CompletionResult::CompletionList(CompletionList {
                items: vec![CompletionItem {
                    label: "some_label".to_string(),
                    insert_text: Some("some_insert_text".to_string()),
                    text_edit: Some(TextEdit {
                        new_text: "some_new_text".to_string(),
                    }),
                }],
            }),
        )),
        error: None,
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn completion_result_completion_items() {
    let json: serde_json::Value = json!({
        "jsonrpc": "2.0".to_string(),
        "id": 123,
        "result": [
            {
                "label": "some_label",
            }
        ]
    });

    let decoded = Message::Response(Response {
        jsonrpc: "2.0".to_string(),
        id: 123,
        result: Some(Result::TextDocumentCompletionResult(
            CompletionResult::CompletionItems(vec![CompletionItem {
                label: "some_label".to_string(),
                insert_text: None,
                text_edit: None,
            }]),
        )),
        error: None,
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn message_request_response_notification_variants() {
    let json: serde_json::Value = json!({
        "jsonrpc": "2.0",
        "id": 123,
        "result": {
            "items": [
                {
                    "label": "some_label",
                }
            ]
        }
    });

    let decoded = Message::Response(Response {
        jsonrpc: "2.0".to_string(),
        id: 123,
        result: Some(Result::TextDocumentCompletionResult(
            CompletionResult::CompletionList(CompletionList {
                items: vec![CompletionItem {
                    label: "some_label".to_string(),
                    insert_text: None,
                    text_edit: None,
                }],
            }),
        )),
        error: None,
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn decode_request() {
    let json: serde_json::Value = json!({
      "jsonrpc": "2.0",
      "id": 1,
      "method": "textDocument/definition",
      "params": {
        "textDocument": {
          "uri": "file:///tiny-lsp-client/test/clangd/main.cpp"
        },
        "position": {
          "line": 10,
          "character": 18
        }
      }
    });

    let decoded = Message::Request(Request {
        jsonrpc: "2.0".to_string(),
        id: 1,
        method: "textDocument/definition".to_string(),
        params: Some(Params::DefinitionParams(DefinitionParams {
            text_document: TextDocumentIdentifier {
                uri: "file:///tiny-lsp-client/test/clangd/main.cpp".to_string(),
            },
            position: Position {
                line: 10,
                character: 18,
            },
        })),
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn decode_request_without_params() {
    let json: serde_json::Value = json!({
      "id": 1,
      "jsonrpc": "2.0",
      "method": "workspace/codeLens/refresh"
    });

    let decoded = Message::Request(Request {
        jsonrpc: "2.0".to_string(),
        id: 1,
        method: "workspace/codeLens/refresh".to_string(),
        params: None,
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn decode_notification() {
    let json: serde_json::Value = json!({
      "jsonrpc": "2.0",
      "method": "textDocument/didOpen",
      "params": {
        "textDocument": {
          "uri": "file:///tiny-lsp-client/test/clangd/main.cpp",
          "languageId": "languageId",
          "version": 0,
          "text": "hej"
        }
      }
    });

    let decoded = Message::Notification(Notification {
        jsonrpc: "2.0".to_string(),
        method: "textDocument/didOpen".to_string(),
        params: Some(Params::DidOpenTextDocumentParams(
            DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: "file:///tiny-lsp-client/test/clangd/main.cpp"
                        .to_string(),
                    language_id: "languageId".to_string(),
                    version: 0,
                    text: "hej".to_string(),
                },
            },
        )),
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn decode_response_with_result() {
    let json: serde_json::Value = json!({
      "id": 1,
      "jsonrpc": "2.0",
      "result": [
        {
          "range": {
            "end": {
              "character": 20,
              "line": 4
            },
            "start": {
              "character": 6,
              "line": 4
            }
          },
          "uri": "file:///tiny-lsp-client/test/clangd/main.cpp"
        }
      ]
    });

    let decoded = Message::Response(Response {
        jsonrpc: "2.0".to_string(),
        id: 1,
        result: Some(Result::TextDocumentDefinitionResult(
            DefinitionResult::LocationList(vec![Location {
                uri: "file:///tiny-lsp-client/test/clangd/main.cpp".to_string(),
                range: Range {
                    start: Position {
                        line: 4,
                        character: 6,
                    },
                    end: Position {
                        line: 4,
                        character: 20,
                    },
                },
            }]),
        )),
        error: None,
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn decode_response_with_error() {
    let json: serde_json::Value = json!({
        "id": 1,
        "jsonrpc": "2.0",
        "error": {
      "code": -32801,
      "message": "msg"
    }
      });

    let decoded = Message::Response(Response {
        jsonrpc: "2.0".to_string(),
        id: 1,
        result: None,
        error: Some(ResponseError {
            code: -32801,
            message: "msg".to_string(),
        }),
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn decode_response_with_result_and_error() {
    let json: serde_json::Value = json!({
      "id": 1,
      "jsonrpc": "2.0",
      "result": [
        {
          "range": {
            "end": {
              "character": 20,
              "line": 4
            },
            "start": {
              "character": 6,
              "line": 4
            }
          },
          "uri": "file:///tiny-lsp-client/test/clangd/main.cpp"
        }
      ],
        "error": {
      "code": -32801,
      "message": "msg"
    }
    });

    let decoded = Message::Response(Response {
        jsonrpc: "2.0".to_string(),
        id: 1,
        result: Some(Result::TextDocumentDefinitionResult(
            DefinitionResult::LocationList(vec![Location {
                uri: "file:///tiny-lsp-client/test/clangd/main.cpp".to_string(),
                range: Range {
                    start: Position {
                        line: 4,
                        character: 6,
                    },
                    end: Position {
                        line: 4,
                        character: 20,
                    },
                },
            }]),
        )),
        error: Some(ResponseError {
            code: -32801,
            message: "msg".to_string(),
        }),
    });

    assert_json_decodes_into(json, decoded);
}

#[test]
fn decode_unkown() {
    // A request/notification has method
    // A response has result OR error (reminder: if both are allowed to be none
    // it will incorrectly be decoded as request)
    let json: serde_json::Value = json!({
      "id": 1,
      "jsonrpc": "2.0",
    });

    let decoded = Message::Unknown(RawMessage {
        jsonrpc: "2.0".to_string(),
        id: Some(1),
        method: None,
        params: None,
        result: None,
        error: None,
    });

    assert_json_decodes_into(json, decoded);
}
