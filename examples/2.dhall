{
  description = "~ is an invalid character",
  args = ["2.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    2.ipso:1:11: error: expected one of: constructor, identifier, '{', '(', '->', '=>', indent (== 0), (|
      |
    1 | main : IO ~
      |           ^
    '',
  exitcode = 1
}
