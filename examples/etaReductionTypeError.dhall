{
  description = "type error for an eta reduced function",
  args = ["etaReductionTypeError.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    etaReductionTypeError.ipso:3:3: error: expected type "Array a -> String", got type "Array a -> a"
      |
    3 |   array.index 0
      |   ^
    '',
  exitcode = 1
}