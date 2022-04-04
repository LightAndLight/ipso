{
  description = "from x import ... where it names a missing item",
  args = ["badExplicitImport/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    badExplicitImport/main.ipso:1:22: error: not defined in module
      |
    1 | from other import a, b, c
      |                      ^
    '',
  exitcode = 1
}
