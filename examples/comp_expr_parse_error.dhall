{
  description = "computation expression parse error",
  args = ["comp_expr_parse_error.ipso"],
  stdin = Some "",
  stdout = "",
  stderr = 
    ''
    comp_expr_parse_error.ipso:4:3: error: expected one of: indent (> 2)
      |
    4 |   bind x <- readLineStdin stdin
      |   ^
    '',
  exitcode = 1
}