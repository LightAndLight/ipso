{
  description = "top level recursion with a type error",
  args = ["recursionTypeError.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    recursionTypeError.ipso:5:23: error: expected type "Int", got type "String"
      |
    5 |   else add 1 (recount "hello")
      |                       ^
    '',
  exitcode = 1
}