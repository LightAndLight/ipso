{
  description = "do notation type error",
  args = ["do_notation_error.ipso"],
  stdin = Some "hello!",
  stdout = "",
  stderr =
    ''
    do_notation_error.ipso:4:22: error: expected type "Bytes", got type "String"
      |
    4 |   writeStdout stdout x
      |                      ^
    '',
  exitcode = 1
}