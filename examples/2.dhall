{
  description = "~ is an invalid character",
  args = ["2.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    /build/source/examples/2.ipso:1:11: error: expected one of: constructor, identifier, "Array", "Bool", "Char", "IO", "Int", "String", '{', '(', '<', '->', '=>', newline, space
      |
    1 | main : IO ~
      |           ^
    '',
  exitcode = 1
}