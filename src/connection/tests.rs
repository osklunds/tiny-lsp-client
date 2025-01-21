
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

    let params = DefinitionParams {
        text_document: TextDocumentIdentifier {
            uri: "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs".to_string()
        },
        position: Position {
            character: 4,
            line: 4
        }
    };

    let params = RequestParams::DefinitionParams(params);

    let mut result: Vec<LocationLink> = vec![];
    loop {
        // this might have to return id, to be used when comparing recv
        // recv should not return Message. Just (usize, Response)
        connection.send_request(params.clone());
        let response = connection.recv_response();

        // rust-analyzer isn't ready to respond to the request immediately due
        // to it indexing first. So try until succeeds.  rust-analyzer responds
        // with different results before it's ready depending on timing.
        if let Some(Result::TextDocumentDefinitionResult(r)) = &response.result {
            if let DefinitionResult::LocationLinkList(r) = r {
                if !r.is_empty() {
                    result = r.to_vec();
                    break;
                }
            }
        }

        thread::sleep(Duration::from_millis(100));
    }

    let location_link = &result[0];
    let target_range = &location_link.target_range;
    let start = &target_range.start;
    let end = &target_range.end;
    assert!(start.line == 7);
    assert!(start.character == 0);
    assert!(end.line == 9);
    assert!(end.character == 1);

    // Send a second identical request and get a response immediately
    connection.send_request(params);
    connection.recv_response();
}
