// Copied/modified from https://ryanfaulhaber.com/posts/first-emacs-module-rust/

extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    let emacs_module_h_bindings = bindgen::Builder::default()
        .header("/usr/include/emacs-module.h")
        .blocklist_function("emacs_module_init")
        .generate()
        .unwrap();

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    emacs_module_h_bindings
        .write_to_file(out_path.join("emacs_module_h_bindings.rs"))
        .unwrap();
}
