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

#[test]
fn uri_test() {
    assert_eq!("hello", file_name_to_uri("hello"));
    assert_eq!("%2Fusr%2Fc%2B%2B%2Fhello", file_name_to_uri("/usr/c++/hello"));
    assert_eq!("abc%C3%A5%C3%A4%C3%B6%E3%81%82%E6%97%A5",
               file_name_to_uri("abcåäöあ日"));
}

fn file_name_to_uri(file_name: &str) -> String {
    let mut uri = String::new();

    for c in file_name.chars() {
        if is_unreserved(c) {
            uri.push(c);
        } else {
            let mut buf = [0; 4];
            let sub_chars = c.encode_utf8(&mut buf);
            for sub_c in sub_chars.as_bytes() {
                uri.push_str(&format!("%{sub_c:X}"));
            }
        }
    }

    uri
}

fn is_unreserved(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '-' || c == '.' || c == '_' || c == '~'
}
