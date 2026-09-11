// Copyright (C) 2025-2026 Oskar Lundström

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

use super::*;
use std::env;

#[test]
fn initialize() {
    logger::set_log_file_name("/tmp/tiny-lsp-client.log");

    let mut server = Server::new(
        &env::current_dir().unwrap().display().to_string(),
        "rust-analyzer",
    )
    .unwrap();

    server.initialize(Duration::from_secs(60));
}
