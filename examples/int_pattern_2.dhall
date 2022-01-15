{
  description = "using another pattern in an int pattern match",
  args = ["int_pattern_2.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    int_pattern_2.ipso:5:5: error: expected type "Int", got type "{ x : ?3 }"
      |
    5 |     { x } -> print "saw { x }"
      |     ^
    '',
  exitcode = 1
}