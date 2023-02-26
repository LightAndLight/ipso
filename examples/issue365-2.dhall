{
  description = "https://github.com/LightAndLight/ipso/issues/365 test 2",
  args = ["issue365-2.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue365-2.ipso:4:1: error: expected one of: indent (> 0)
      |
    4 | Object : { hash : String },
      | ^
    '',
  exitcode = 1
}