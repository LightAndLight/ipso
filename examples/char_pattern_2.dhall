{
  description = "using another pattern in a char pattern match",
  args = ["char_pattern_2.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    char_pattern_2.ipso:5:5: error: expected type "Char", got type "{ x : ?4 }"
      |
    5 |     { x } -> print "saw { x }"
      |     ^
    '',
  exitcode = 1
}