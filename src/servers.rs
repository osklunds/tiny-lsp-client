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

use std::borrow::BorrowMut;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::server::Server;

thread_local! {
    static SERVERS: RefCell<HashMap<ServerKey, Server>> =
        RefCell::new(HashMap::new());
}

// Run a function with all servers as arg. Note that if you only need one
// server, use with_server() instead, since it doesn't check is_working()
// on ALL servers, and is thus faster.
pub fn with_servers<F, R>(f: F) -> R
where
    F: FnOnce(&mut HashMap<ServerKey, Server>) -> R,
{
    SERVERS.with_borrow_mut(|servers| {
        servers.retain(|_root_path, server| server.is_working());
        f(servers.borrow_mut())
    })
}

pub fn with_server<F, R>(function: F, server_key: ServerKey, default: R) -> R
where
    F: FnOnce(&mut Server) -> Option<R>,
{
    SERVERS.with_borrow_mut(|servers| {
        let result = if let Some(ref mut server) = servers.get_mut(&server_key)
        {
            if server.is_working() {
                if let Some(result) = function(server) {
                    Some(result)
                } else {
                    // This means the function thinks the server is supposed to
                    // be stopped, e.g. if recv_response() indicated an error.
                    None
                }
            } else {
                // This means the server wasn't existing before this call
                None
            }
        } else {
            // This means the server existed, but was already dead
            None
        };

        if let Some(result) = result {
            result
        } else {
            servers.remove(&server_key);
            default
        }
    })
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Ord, PartialOrd)]
pub struct ServerKey {
    pub root_path: String,
    pub server_cmd: String,
}
