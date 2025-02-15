use std::path::Component;
use std::process::Command;

#[test]
fn hej() {
    build(false);
    build(true);

    run_lisp_file();
}

fn run_lisp_file() {
    let mut args = vec![
        "-Q",
        "--batch",
        "--eval",
        "(load-file \"test/tiny-lsp-client-lisp-bindings-test.el\")",
    ];

    let mut child = Command::new("emacs").args(args).spawn().unwrap();
    let status = child.wait().unwrap();
    assert!(status.success());
}

fn build(release: bool) {
    assert_in_repo();
    let mut args = vec!["build"];
    if release {
        args.push("--release");
    }

    let mut process = Command::new("cargo").args(args).spawn().unwrap();
    assert!(process.wait().unwrap().success());
}

fn assert_in_repo() {
    // Test that current dir seems to be the root of the repo
    let current_dir = std::env::current_dir().unwrap();
    let path_components: Vec<_> = current_dir.components().collect();
    let last = path_components.last().unwrap();
    assert_eq!(&Component::Normal("tiny-lsp-client".as_ref()), last);
}
