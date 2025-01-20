
use super::*;

#[test]
fn initialize() {
    let mut connection = Connection::new(
        "rust-analyzer",
        "/home/oskar/own_repos/tiny-lsp-client"
    );

    connection.initialize();
}

#[test]
fn text_document_definition() {
    let mut connection = Connection::new(
        "rust-analyzer",
        "/home/oskar/own_repos/tiny-lsp-client"
    );

    connection.initialize();

    let params = TextDocumentDefinitionRequestParams {
        text_document: TextDocumentIdentifier {
            uri: "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs".to_string()
        },
        position: Position {
            character: 4,
            line: 4
        }
    };

    // todo: instead of sleep, do busy loop with 1ms
    thread::sleep(Duration::from_secs(1));

    // this might have to return id, to be used when comparing recv
    // recv should not return Message. Just (usize, Response)
    connection.send_request(Params::TextDocumentDefinitionRequestParams(params));
    let response = connection.recv_response();
    let Result::TextDocumentDefinitionResult(result) = &response.result else {
        panic!();
    };
    let TextDocumentDefinitionResult::LocationLinkList(result) = &result else {
        panic!();
    };
    assert!(result.len() == 1);

    let location_link = &result[0];
    let target_range = &location_link.target_range;
    let start = &target_range.start;
    let end = &target_range.end;
    assert!(start.line == 7);
    assert!(start.character == 0);
    assert!(end.line == 9);
    assert!(end.character == 1);
}
