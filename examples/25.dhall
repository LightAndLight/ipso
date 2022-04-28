{
  description = "simple type mismatch in function argument",
  args = ["25.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    25.ipso:15:9: error: expected type "String", got type "a"
       |
    15 |         (array.index 0 arr)
       |         ^
    '',
  exitcode = 1
}
