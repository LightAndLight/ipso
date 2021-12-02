{
  description = "computation expression ends with bind error",
  args = ["comp_expr_bind_end.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    comp_expr_bind_end.ipso:2:8: error: computation expression ends with a bind
      |
    2 | main = comp
      |        ^
    '',
  exitcode = 1
}