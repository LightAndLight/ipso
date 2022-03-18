{
  description = "using an int pattern when matching on another type",
  args = ["int_pattern_1.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    int_pattern_1.ipso:4:5: error: expected type "Char", got type "Int"
      |
    4 |     1 -> print "saw 1"
      |     ^
    '',
  exitcode = 1
}