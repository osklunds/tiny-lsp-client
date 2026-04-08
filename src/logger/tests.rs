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

const TEST_LOG_FILE_NAME: &str = "test/logs/logger_tests.rs.log";

#[test]
fn timestamp() {
    let timestamp = get_timestamp();

    println!("Timestamp: {}", timestamp);
    // No need to make it overly smart, because no matter how smart,
    // it will never be able to compare against the real time with a regex
    match_regex(
        r"^2[0-9]{3}-[01][0-9]-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}$",
        &timestamp,
    );
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

// TODO: there is a lot more to test, like if a log is enabled or not, changing
// log file, etc
#[test]
fn log() {
    set_log_file_name(TEST_LOG_FILE_NAME);
    log_io!("abc");
    log_stderr!("def");
    log_rust_debug!("hij");
    log_emacs_debug!("klm");

    let log_file_content = fs::read_to_string(TEST_LOG_FILE_NAME).unwrap();

    match_regex(r"^IO - [0-9-]+ [0-9:.]+ - abc\n", &log_file_content);
    match_regex(r"\nSTDERR - [0-9-]+ [0-9:.]+ - def\n", &log_file_content);
    match_regex(
        r"\nRUST_DEBUG - [0-9-]+ [0-9:.]+ - hij\n",
        &log_file_content,
    );
    match_regex(
        r"\nEMACS_DEBUG - [0-9-]+ [0-9:.]+ - klm\n$",
        &log_file_content,
    );

    match_regex(
        r"IO[^^]*STDERR[^^]*RUST_DEBUG[^^]*EMACS_DEBUG",
        &log_file_content,
    );
}

#[test]
fn unicode_truncation() {
    set_log_file_name(TEST_LOG_FILE_NAME);

    // To force split at unicode boundary
    for i in 0..4 {
        let mut log_entry = "a".repeat(MAX_LOG_ENTRY_LEN_BYTES / 2 + i);
        log_entry.push_str(&"あ".repeat(MAX_LOG_ENTRY_LEN_BYTES / 2));

        log_io!("{}", log_entry);

        let log_file_content = fs::read_to_string(TEST_LOG_FILE_NAME).unwrap();

        match_regex(r"a+あ+\.\.\.TRUNCATED\n$", &log_file_content);
    }
}

fn match_regex(regex: &str, subject: &str) {
    let regex = Regex::new(regex).unwrap();

    assert!(regex.is_match(subject));
}
