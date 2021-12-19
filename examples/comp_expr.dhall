{
  description = "computation expression test",
  args = ["comp_expr.ipso"],
  stdin = Some "hello\nworld!",
  stdout = "hello\nworld!",
  stderr = "",
  exitcode = 0
}