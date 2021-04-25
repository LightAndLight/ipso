{
  args = ["28/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
  ''
  28/main.ipso:4:35: error: expected type "String", got type "Int"
    |
  4 | main = writeStdout stdout (toUtf8 greeting.value)
    |                                   ^
  '',
  exitcode = 1
}
