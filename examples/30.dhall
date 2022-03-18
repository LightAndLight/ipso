{
  description = "not in scope error after string containing escape sequences",
  args = ["30.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    30.ipso:5:8: error: variable not in scope
      |
    5 | main = hi
      |        ^
    '',
  exitcode = 1
}
