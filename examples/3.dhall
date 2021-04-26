{
  description = "not in scope error",
  args = ["3.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    /build/source/examples/3.ipso:2:8: error: not in scope
      |
    2 | main = hello
      |        ^
    '',
  exitcode = 1
}