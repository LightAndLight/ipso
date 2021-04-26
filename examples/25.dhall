{
  description = "simple type mismatch in function argument",
  args = ["25.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    /build/source/examples/25.ipso:15:9: error: expected type "String", got type "a"
       |
    15 |         (indexArray 0 arr)
       |         ^
    '',
  exitcode = 1
}
