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

use std::cell::RefCell;
use std::collections::HashMap;

use crate::server::Server;

thread_local! {
    static SERVERS: RefCell<Option<HashMap<String, Server>>> =
        RefCell::new(None);
}

pub fn with_servers<F, R>(f: F) -> R
where
    F: FnOnce(&mut HashMap<String, Server>) -> R,
{
    SERVERS.with_borrow_mut(|servers| {
        // todo: consider performance of always calling retain
        // unclear why get_or_insert() didn't work
        if servers.is_none() {
            *servers = Some(HashMap::new());
        }
        servers
            .as_mut()
            .unwrap()
            .retain(|_root_path, server| server.is_working());
        f(servers.as_mut().unwrap())
    })
}
