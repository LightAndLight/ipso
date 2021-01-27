{
  args = ["22.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    22.ipso:15:39: error: cannot deduce "A (Array Int)"
       |
    15 |   bindIO (writeStdout stdout (toUtf8 (a [1, 2, 3]))) (\_ ->
       |                                       ^
    '',
  exitcode = 1
}