use super::*;

use serde_json::json;

#[test]
fn completion_result() {
    let untyped: serde_json::Value = json!({
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
    let json: String = serde_json::to_string(&untyped).unwrap();
    let typed: Message = serde_json::from_str(&json).unwrap();

    let exp = Message::Response(Response {
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

    assert_eq!(exp, typed);
}
