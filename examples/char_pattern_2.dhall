{
  description = "using another pattern in a char pattern match",
  args = ["char_pattern_2.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    char_pattern_2.ipso:5:5: error: expected type "Char", got type "{ x : ?8 }"
      |
    5 |     { x } -> writeStdout stdout (toUtf8 "saw { x }")
      |     ^
    '',
  exitcode = 1
}