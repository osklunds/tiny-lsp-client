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
