{
  description = "computation expression ends with bind error",
  args = ["comp_expr_bind_end.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    comp_expr_bind_end.ipso:3:3: error: computation expression ends with a bind
      |
    3 |   comp {
      |   ^
    '',
  exitcode = 1
}