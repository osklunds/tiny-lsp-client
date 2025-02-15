use std::process::Command;
use std::path::Component;

#[test]
fn hej() {
    build();
}

fn build() {
    let mut build = Command::new("cargo").args(vec!["build"]).spawn().unwrap();
    assert!(build.wait().unwrap().success());
}

fn assert_in_repo() {
    // Test that current dir seems to be the root of the repo
    let current_dir = std::env::current_dir().unwrap();
    let path_components: Vec<_> = current_dir.components().collect();
    let last = path_components.last().unwrap();
    assert_eq!(&Component::Normal("tiny-lsp-client".as_ref()), last);
}
