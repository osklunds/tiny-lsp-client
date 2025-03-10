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

use super::*;

use regex::Regex;

#[test]
fn timestamp() {
    let timestamp = get_timestamp();

    println!("Timestamp: {}", timestamp);
    // No need to make it overly smart, because no matter how smart,
    // it will never be able to compare against the real time with a regex
    let regex = Regex::new(
        r"^2[0-9]{3}-[01][0-9]-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}$",
    )
    .unwrap();
    assert!(regex.is_match(&timestamp));
}

#[test]
fn millisecond_padding() {
    // Test that padding seems to work correctly by testing if they can occur at
    // all.

    let regexs = vec![
        r"\.000$",
        r"\.[1-9]00$",
        r"\.[1-9]{2}0$",
        r"\.[1-9]{3}$",
        r"\.0[1-9]{2}$",
        r"\.00[1-9]$",
        r"\.00[7-8]$", // 1s seem to be favored so force something else
    ];

    for regex in regexs {
        let regex = Regex::new(regex).unwrap();
        loop {
            let timestamp = get_timestamp();
            if regex.is_match(&timestamp) {
                println!("Timestamp: {}", timestamp);
                break;
            }
        }
    }
}
