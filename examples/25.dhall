{
  description = "simple type mismatch in function argument",
  args = ["25.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    25.ipso:15:24: error: expected type "Array String", got type "Array a"
       |
    15 |         (array.index 0 arr)
       |                        ^
    '',
  exitcode = 1
}
