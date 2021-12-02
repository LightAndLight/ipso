{
  description = "computation expression type error - on RHS of bind",
  args = ["comp_expr_error_2.ipso"],
  stdin = Some "hello!",
  stdout = "",
  stderr =
    ''
    comp_expr_error_2.ipso:3:13: error: expected type "IO ?1", got type "Int"
      |
    3 |   bind x <- 666
      |             ^
    '',
  exitcode = 1
}