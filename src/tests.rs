use std::path::Component;
use std::process::Command;

#[test]
fn lisp_bindings() {
    build(true);
    run_lisp_file("test/lisp-bindings-test.el", true);
}

#[test]
fn mode_test() {
    build(false);
    run_lisp_file("test/mode-test.el", true);
}

// Since interactive and manual, need to ignore from normal runs
#[ignore]
#[test]
fn interactive_test() {
    build(true); // release, since tiny-lsp-client loads the release binary
    run_lisp_file("test/interactive-test.el", false);
}

#[test]
fn erlang_ls_test() {
    build(false);
    run_lisp_file("test/erlang-ls-test.el", true);
}

#[test]
fn clangd_test() {
    build(false);
    run_lisp_file("test/clangd-test.el", true);
}

fn run_lisp_file<S: AsRef<str>>(lisp_file_name: S, batch: bool) {
    let eval_arg = format!("(load-file \"{}\")", lisp_file_name.as_ref());
    let mut args = vec!["-Q", "--eval", &eval_arg];
    if batch {
        args.push("--batch");
    }

    let mut child = Command::new("emacs")
        .args(args)
        .env("RUST_BACKTRACE", "full")
        .spawn()
        .unwrap();
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
