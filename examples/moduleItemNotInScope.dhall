{
  description = "error message when a module item is not in scope",
  args = ["moduleItemNotInScope/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    moduleItemNotInScope/main.ipso:4:18: error: variable not in scope
      |
    4 | main = println a.b
      |                  ^
    '',
  exitcode = 1
}