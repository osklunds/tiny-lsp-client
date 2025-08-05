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

// @credits: This module as a whole is inspired by the msg module of
// https://github.com/zbelial/lspce

#[cfg(test)]
mod tests;

use serde::Deserialize;
use serde::Serialize;

// Keeping a simple constant until I see signs it needs to be dynamic
pub const LANGUAGE_ID: &'static str = "languageId";

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct Request {
    pub jsonrpc: String,
    pub id: u32,
    pub method: String,
    pub params: RequestParams,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum RequestParams {
    DefinitionParams(DefinitionParams),
    CompletionParams(CompletionParams),
    HoverParams(HoverParams),
    Untyped(serde_json::Value),
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct DefinitionParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct TextDocumentIdentifier {
    pub uri: String,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(rename = "start")]
pub struct Position {
    pub line: usize,
    pub character: usize,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct CompletionParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
    pub context: CompletionContext,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct CompletionContext {
    #[serde(rename = "triggerKind")]
    pub trigger_kind: usize, // todo: enum
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
    pub message: String,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Result {
    TextDocumentDefinitionResult(DefinitionResult),
    TextDocumentCompletionResult(CompletionResult),
    TextDocumentHoverResult(HoverResult),
    Untyped(serde_json::Value),
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum DefinitionResult {
    LocationList(Vec<Location>),
    LocationLinks(Vec<LocationLink>),
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct Location {
    pub uri: String,
    pub range: Range,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct LocationLink {
    #[serde(rename = "targetUri")]
    pub target_uri: String,
    #[serde(rename = "targetRange")]
    pub target_range: Range,
    #[serde(rename = "targetSelectionRange")]
    pub target_selection_range: Range,
}

impl LocationLink {
    pub fn to_location(self) -> Location {
        Location {
            uri: self.target_uri,
            range: self.target_selection_range,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum CompletionResult {
    CompletionList(CompletionList),
    CompletionItems(Vec<CompletionItem>),
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct CompletionList {
    pub items: Vec<CompletionItem>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct CompletionItem {
    pub label: String,
    #[serde(rename = "insertText")]
    pub insert_text: Option<String>,
    #[serde(rename = "textEdit")]
    pub text_edit: Option<TextEdit>,
}

impl CompletionItem {
    pub fn get_text(&self) -> &str {
        if let Some(text_edit) = &self.text_edit {
            &text_edit.new_text
        } else if let Some(insert_text) = &self.insert_text {
            &insert_text
        } else {
            &self.label
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct TextEdit {
    // todo: serde might had some option to camel case all fields
    #[serde(rename = "newText")]
    pub new_text: String,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct Range {
    pub start: Position,
    pub end: Position,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct Notification {
    pub jsonrpc: String,
    pub method: String,
    pub params: NotificationParams,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum NotificationParams {
    DidOpenTextDocumentParams(DidOpenTextDocumentParams),
    DidChangeTextDocumentParams(DidChangeTextDocumentParams),
    DidCloseTextDocumentParams(DidCloseTextDocumentParams),
    Untyped(serde_json::Value),
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
    pub text: String,
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
    pub content_changes: Vec<TextDocumentContentChangeEvent>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct VersionedTextDocumentIdentifier {
    pub uri: String,
    pub version: isize,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum TextDocumentContentChangeEvent {
    TextDocumentContentChangeEventIncremental(
        TextDocumentContentChangeEventIncremental,
    ),
    TextDocumentContentChangeEventFull(TextDocumentContentChangeEventFull),
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct TextDocumentContentChangeEventIncremental {
    pub range: Range,
    pub text: String,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct TextDocumentContentChangeEventFull {
    pub text: String,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct HoverParams {
    #[serde(rename = "textDocument")]
    pub text_document: TextDocumentIdentifier,
    pub position: Position,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct HoverResult {
    pub contents: MarkupContent,
}

#[derive(PartialEq, Debug, Serialize, Deserialize, Clone)]
pub struct MarkupContent {
    pub value: String
}
