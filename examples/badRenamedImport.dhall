{
  description = "import a as b ... a",
  args = ["badRenamedImport/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    badRenamedImport/main.ipso:4:16: error: variable not in scope
      |
    4 | main = println a.value
      |                ^
    '',
  exitcode = 1
}
