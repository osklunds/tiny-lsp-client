use super::*;

use serde_json::json;

#[test]
fn completion_result() {
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
        result: Some(Result::TextDocumentCompletionResult(CompletionResult {
            items: vec![CompletionItem {
                label: "some_label".to_string(),
                insert_text: None,
                text_edit: None,
            }],
        })),
        error: None,
    });

    assert_json_decodes_into(json, decoded);
}

fn assert_json_decodes_into(json: serde_json::Value, exp_decoded: Message) {
    let encoded: String = serde_json::to_string(&json).unwrap();
    let decoded: Message = serde_json::from_str(&encoded).unwrap();
    assert_eq!(exp_decoded, decoded);
}
