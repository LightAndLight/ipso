#[cfg(test)]
use crate::{Diagnostic, Message};

#[test]
fn test_1() {
    assert_eq!(
        Diagnostic::report_located_message(
            5,
            3,
            "test/file",
            &String::from("abcdefg"),
            &Message {
                content: String::from("some error"),
                addendum: None
            },
        ),
        [
            "test/file:5:3: error: some error",
            "  |",
            "5 | abcdefg",
            "  |   ^",
        ]
        .join("\n")
    )
}

#[test]
fn test_2() {
    assert_eq!(
        Diagnostic::report_located_message(
            10,
            2,
            "test/file",
            &String::from("abcdefg"),
            &Message {
                content: String::from("some error"),
                addendum: None
            }
        ),
        [
            "test/file:10:2: error: some error",
            "   |",
            "10 | abcdefg",
            "   |  ^",
        ]
        .join("\n")
    )
}
