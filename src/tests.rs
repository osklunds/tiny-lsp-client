use std::path::Component;
use std::process::Command;

#[test]
fn lisp_bindings() {
    build(false);
    build(true);

    run_lisp_file("test/lisp-bindings-test.el");
}

fn run_lisp_file<S: AsRef<str>>(lisp_file_name: S) {
    let eval_arg = format!("(load-file \"{}\")", lisp_file_name.as_ref());
    let mut args = vec![
        "-Q",
        "--batch",
        "--eval",
        &eval_arg
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
