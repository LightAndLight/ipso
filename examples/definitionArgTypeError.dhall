{
  description = "type error locations for definition arguments",
  args = ["definitionArgTypeError.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    definitionArgTypeError.ipso:2:6: error: expected type "Int", got type "Char"
      |
    2 | test 'c' = true
      |      ^
    '',
  exitcode = 1
}