{
  description = "computation expression with a let binding as the last line",
  args = ["comp_expr_let_last.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    comp_expr_let_last.ipso:7:5: error: computation expression ends with a let
      |
    7 |     let c = "3"
      |     ^
    '',
  exitcode = 1
}