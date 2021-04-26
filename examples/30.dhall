{
  description = "not in scope error after string containing escape sequences",
  args = ["30.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    /build/source/examples/30.ipso:5:8: error: not in scope
      |
    5 | main = hi
      |        ^
    '',
  exitcode = 1
}
