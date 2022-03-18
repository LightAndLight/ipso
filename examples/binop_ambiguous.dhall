{
  description = "ambiguous binary operator usage",
  args = ["binop_ambiguous.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    binop_ambiguous.ipso:2:22: error: ambiguous use of ==
      |
    2 | main = print (a == b == c)
      |                      ^
    '',
  exitcode = 1
}