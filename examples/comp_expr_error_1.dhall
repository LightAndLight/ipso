{
  description = "computation expression type error - in expression line",
  args = ["comp_expr_error_1.ipso"],
  stdin = Some "hello!",
  stdout = "",
  stderr =
    ''
    comp_expr_error_1.ipso:5:24: error: expected type "Bytes", got type "String"
      |
    5 |     writeStdout stdout x
      |                        ^
    '',
  exitcode = 1
}