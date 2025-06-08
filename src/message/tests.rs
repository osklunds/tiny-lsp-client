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
        "jsonrpc": 2.0,
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
        "jsonrpc": 2.0,
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
        "jsonrpc": 2.0,
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
        "jsonrpc": 2.0,
        "id": 123,
        "result": [
            {
                "label": "some_label",
            }
        ]
    });

    let decoded = Message::Response(Response {
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
