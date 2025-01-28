
use serde_json::{json, Value, Number};
use serde::Deserialize;
use serde::Serialize;

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification)
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct Request {
    pub id: u32,
    pub method: String,
    pub params: RequestParams,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum RequestParams {
    DefinitionParams(DefinitionParams),
    Untyped(serde_json::Value)
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct DefinitionParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentIdentifier,
    pub position: Position
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct TextDocumentIdentifier {
    pub uri: String
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(rename = "start")]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct Response {
    pub id: u32,
    pub result: Option<Result>,
    pub error: Option<ResponseError>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct ResponseError {
    pub code: isize, // todo: strongly typed enum
    pub message: String
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Result {
    TextDocumentDefinitionResult(DefinitionResult),
    Untyped(serde_json::Value)
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum DefinitionResult {
    LocationLinkList(Vec<LocationLink>),
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct LocationLink {
    #[serde(rename = "targetUri")]
    pub target_uri: String,
    #[serde(rename = "targetRange")]
    pub target_range: Range,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct Range {
    pub start: Position,
    pub end: Position
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct Notification {
    pub method: String,
    pub params: NotificationParams
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum NotificationParams {
    DidOpenTextDocumentParams(DidOpenTextDocumentParams),
    DidChangeTextDocumentParams(DidChangeTextDocumentParams),
    DidCloseTextDocumentParams(DidCloseTextDocumentParams),
    Untyped(serde_json::Value)
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct DidOpenTextDocumentParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentItem,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct TextDocumentItem {
    pub uri: String,
    #[serde(rename = "languageId")]
    pub language_id: String,
    pub version: isize,
    pub text: String
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct DidCloseTextDocumentParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentIdentifier,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct DidChangeTextDocumentParams {
    #[serde(rename = "textDocument")]
    pub text_document: VersionedTextDocumentIdentifier,
    #[serde(rename = "contentChanges")]
    pub content_changes: Vec<TextDocumentContentChangeEvent>
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct VersionedTextDocumentIdentifier {
    pub uri: String,
    pub version: isize
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct TextDocumentContentChangeEvent {
    pub range: Range,
    pub text: String
}

