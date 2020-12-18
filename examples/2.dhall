{
  args = ["2.ipso"],
  stdout = "",
  stderr = 
    ''
    2.ipso:1:11: error: expected one of: identifier, "Array", "Bool", "Char", "IO", "Int", "String", '(', '->', '=>', newline, space
      |
    1 | main : IO ~
      |           ^
    '',
  exitcode = 1
}