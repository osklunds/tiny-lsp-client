use std::cell::UnsafeCell;
use std::collections::HashMap;

use crate::connection::Connection;

// Why like this with a special unsafe cell? static variables need to be
// immutable and Sync. But CONNECTIONS is only accessed by one thread, namely,
// emacs' main thread. So avoid Mutex performance penalty by having some
// unsafe functions. Caller (lib.rs) needs to ensure it really only is one
// thread and that multiple calls to connections() aren't made before
// the return is dropped.

// Is this good enough? Maybe. If I notice issues, just revert the commit :)

static CONNECTIONS: MyCell<Option<HashMap<String, Connection>>> =
    MyCell::new(None);

struct MyCell<T> {
    unsafe_cell: UnsafeCell<T>,
}

unsafe impl<T> Sync for MyCell<T> {}

impl<T> MyCell<T> {
    pub const fn new(value: T) -> MyCell<T> {
        MyCell {
            unsafe_cell: UnsafeCell::new(value),
        }
    }

    pub unsafe fn get_mut(&self) -> &mut T {
        &mut *self.unsafe_cell.get()
    }
}

pub unsafe fn connections() -> &'static mut HashMap<String, Connection> {
    let connections = CONNECTIONS.get_mut().get_or_insert(HashMap::new());
    // todo: consider performance of always calling retain
    connections.retain(|_root_path, connection| connection.is_working());
    connections
}
