{
  description = "use an imported value in a way that causes a type error",
  args = ["28/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    /build/source/examples/28/main.ipso:4:35: error: expected type "String", got type "Int"
      |
    4 | main = writeStdout stdout (toUtf8 greeting.value)
      |                                   ^
    '',
  exitcode = 1
}
