
use serde_json::{json, Value, Number};
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification)
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Request {
    pub id: u32,
    pub method: String,
    pub params: Params,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Params {
    TextDocumentDefinitionRequestParams(TextDocumentDefinitionRequestParams),
    Untyped(serde_json::Value)
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TextDocumentDefinitionRequestParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentIdentifier,
    pub position: Position
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct TextDocumentIdentifier {
    pub uri: String
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(rename = "start")]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Response {
    pub id: u32,
    pub result: Result,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Result {
    TextDocumentDefinitionResult(TextDocumentDefinitionResult),
    Untyped(serde_json::Value)
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum TextDocumentDefinitionResult {
    LocationLinkList(Vec<LocationLink>),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct LocationLink {
    #[serde(rename = "targetUri")]
    pub target_uri: String,
    #[serde(rename = "targetRange")]
    pub target_range: Range,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Range {
    pub start: Position,
    pub end: Position
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Notification {
    pub method: String,
    pub params: serde_json::Value,
}

