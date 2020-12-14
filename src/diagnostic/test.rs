#[cfg(test)]
use super::{Diagnostic, Item};

#[test]
fn test_1() {
    assert_eq!(
        Diagnostic::report_string(
            5,
            3,
            "test/file",
            &String::from("abcdefg"),
            &String::from("some error")
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
        Diagnostic::report_string(
            10,
            2,
            "test/file",
            &String::from("abcdefg"),
            &String::from("some error")
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
