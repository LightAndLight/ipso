{
  description = "given `class A x` and `instance A x => A (Array x)`, you don't have `A (Array x)` when you don't have `A x`.",
  args = ["22.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    /build/source/examples/22.ipso:15:39: error: cannot deduce "A (Array Int)"
       |
    15 |   bindIO (writeStdout stdout (toUtf8 (a [1, 2, 3]))) (\_ ->
       |                                       ^
    '',
  exitcode = 1
}