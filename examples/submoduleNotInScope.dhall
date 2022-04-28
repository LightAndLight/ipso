{
  description = "error message when a submodule is not in scope",
  args = ["submoduleNotInScope/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    submoduleNotInScope/main.ipso:4:18: error: variable not in scope
      |
    4 | main = println a.b.c
      |                  ^
    '',
  exitcode = 1
}