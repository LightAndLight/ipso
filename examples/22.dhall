{
  description = "given `class A x` and `instance A x => A (Array x)`, you don't have `A (Array x)` when you don't have `A x`.",
  args = ["22.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    22.ipso:15:22: error: cannot deduce "A (Array Int)"
       |
    15 |   io.andThen (print (a [1, 2, 3])) (\_ ->
       |                      ^
    '',
  exitcode = 1
}