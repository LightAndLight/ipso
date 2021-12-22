{
  description = "using a string pattern when matching on another type",
  args = ["string_pattern_1.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    string_pattern_1.ipso:4:5: error: expected type "Char", got type "String"
      |
    4 |     "hello" -> print "saw hello"
      |     ^
    '',
  exitcode = 1
}