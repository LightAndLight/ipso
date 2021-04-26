{
  description = "composite type mismatch in function argument",
  args = ["7.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    /build/source/examples/7.ipso:2:37: error: expected type "String -> IO ?2", got type "Bytes -> IO ()"
      |
    2 | main = bindIO (readLineStdin stdin) (writeStdout stdout)
      |                                     ^
    '',
  exitcode = 1
}