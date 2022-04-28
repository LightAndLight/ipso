{
  description = "error message when a submodule is not in scope",
  args = ["submoduleNotInScope/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    submoduleNotInScope/main.ipso:2:18: error: variable not in scope
      |
    2 | main = println a.b.c
      |                  ^
    '',
  exitcode = 1
}