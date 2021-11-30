{
  description = "empty computation expression error",
  args = ["comp_expr_empty.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    comp_expr_empty.ipso:2:8: error: empty computation expression
      |
    2 | main = comp {}
      |        ^
    '',
  exitcode = 1
}