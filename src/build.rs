// Copyright (C) 2025 Oskar Lundström

// This file is part of tiny-lsp-client.

// tiny-lsp-client is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.

// tiny-lsp-client is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.

// You should have received a copy of the GNU General Public License along with
// tiny-lsp-client. If not, see <https://www.gnu.org/licenses/>.

// @credits. This module was written with the help of
// https://ryanfaulhaber.com/posts/first-emacs-module-rust/

extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src/");
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
