{
  description = "errors in the presence of single line comments",
  args = ["commentsErrors.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    commentsErrors.ipso:8:5: error: variable not in scope
      |
    8 |     stdooout
      |     ^
    '',
  exitcode = 1
}