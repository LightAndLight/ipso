{
  args = ["7.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    7.ipso:2:37: error: expected type "String -> IO ?0", got type "Bytes -> IO ()"
      |
    2 | main = bindIO (readLineStdin stdin) (writeStdout stdout)
      |                                     ^
    '',
  exitcode = 1
}