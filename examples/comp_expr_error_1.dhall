{
  description = "computation expression type error - in expression line",
  args = ["comp_expr_error_1.ipso"],
  stdin = Some "hello!",
  stdout = "",
  stderr =
    ''
    comp_expr_error_1.ipso:5:11: error: expected type "String", got type "Int"
      |
    5 |     print 1
      |           ^
    '',
  exitcode = 1
}