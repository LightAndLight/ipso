{
  description = "using a char pattern when matching on another type",
  args = ["char_pattern_1.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    char_pattern_1.ipso:4:5: error: expected type "Int", got type "Char"
      |
    4 |     'a' -> print "saw a"
      |     ^
    '',
  exitcode = 1
}