use std::cell::RefCell;
use std::collections::HashMap;

use crate::connection::Connection;

thread_local! {
    static CONNECTIONS: RefCell<Option<HashMap<String, Connection>>> =
        RefCell::new(None);
}

pub fn with_connections<F, R>(f: F) -> R
where
    F: FnOnce(&mut HashMap<String, Connection>) -> R,
{
    CONNECTIONS.with_borrow_mut(|connections| {
        // todo: consider performance of always calling retain
        // unclear why get_or_insert() didn't work
        if connections.is_none() {
            *connections = Some(HashMap::new());
        }
        connections
            .as_mut()
            .unwrap()
            .retain(|_root_path, connection| connection.is_working());
        f(connections.as_mut().unwrap())
    })
}
