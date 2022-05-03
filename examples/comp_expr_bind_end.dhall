{
  description = "computation expression ends with bind error",
  args = ["comp_expr_bind_end.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    comp_expr_bind_end.ipso:4:5: error: computation expression ends with a bind
      |
    4 |     bind x <- readln
      |     ^
    '',
  exitcode = 1
}