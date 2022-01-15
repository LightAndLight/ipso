{
  description = "using another pattern in an string pattern match",
  args = ["string_pattern_2.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    string_pattern_2.ipso:5:5: error: expected type "String", got type "{ x : ?3 }"
      |
    5 |     { x } -> print "saw { x }"
      |     ^
    '',
  exitcode = 1
}