{
  description = "type error in imported module",
  args = ["29/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
  ''
  /build/source/examples/29/greeting.ipso:2:9: error: expected type "String", got type "Int"
    |
  2 | value = 42
    |         ^
  '',
  exitcode = 1
}
