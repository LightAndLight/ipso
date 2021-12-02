{
  description = "computation expression type error - in expression line",
  args = ["comp_expr_error_1.ipso"],
  stdin = Some "hello!",
  stdout = "",
  stderr =
    ''
    comp_expr_error_1.ipso:4:22: error: expected type "Bytes", got type "String"
      |
    4 |   writeStdout stdout x
      |                      ^
    '',
  exitcode = 1
}