{
  description = "leaving parens off instance assumptions causes parse error",
  args = ["instance_parse_error.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    instance_parse_error.ipso:7:17: error: expected one of: comment, constructor, identifier, where, '{', '(', (|
      |
    7 | instance MyEq a => MyEq (Array a) where
      |                 ^
    '',
  exitcode = 1
}