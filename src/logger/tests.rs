use super::*;

use regex::Regex;

#[test]
fn timestamp_test() {
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
