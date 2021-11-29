{
  description = "computation expression type error",
  args = ["comp_expr_error.ipso"],
  stdin = Some "hello!",
  stdout = "",
  stderr =
    ''
    comp_expr_error.ipso:4:24: error: expected type "Bytes", got type "String"
      |
    5 |     writeStdout stdout x
      |                        ^
    '',
  exitcode = 1
}