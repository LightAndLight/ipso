{
  description = "computation expression with a let binding as the last line",
  args = ["comp_expr_let_last.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    comp_expr_let_last.ipso:3:3: error: computation expression ends with a let
      |
    3 |   comp
      |   ^
    '',
  exitcode = 1
}