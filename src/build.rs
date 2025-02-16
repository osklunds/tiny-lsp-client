// Copied/modified from https://ryanfaulhaber.com/posts/first-emacs-module-rust/

extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    let bindings = bindgen::Builder::default()
        .header("/usr/include/emacs-module.h")
        .allowlist_type("emacs_env")
        .allowlist_type("emacs_value")
        .allowlist_type("emacs_runtime")
        .blocklist_function("emacs_module_init")
        .generate()
        .unwrap();

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .unwrap();
}
