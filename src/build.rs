// Copied/modified from https://ryanfaulhaber.com/posts/first-emacs-module-rust/

extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    let bindings = bindgen::Builder::default()
        .header(emacs_module_header_path())
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

fn emacs_module_header_path() -> String {
    if let Ok(location) = env::var("TLC_EMACS_MODULE_HEADER_PATH") {
        location
    } else {
        "/usr/include/emacs-module.h".to_string()
    }
}
