
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

    let params = TextDocumentDefinitionRequest {
        text_document: TextDocumentIdentifier {
            uri: "file:///home/oskar/own_repos/tiny-lsp-client/src/dummy.rs".to_string()
        },
        position: Position {
            character: 4,
            line: 4
        }
    };

    thread::sleep(Duration::from_secs(1));

    connection.send_request(Params::TextDocumentDefinitionRequest(params));
    let response = connection.recv_response();
    println!("oskar: {:?}", response);
}
