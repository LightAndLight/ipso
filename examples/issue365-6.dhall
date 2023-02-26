{
  description = "https://github.com/LightAndLight/ipso/issues/365 test 6",
  args = ["issue365-6.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue365-6.ipso:7:1: error: expected one of: indent (> 0)
      |
    7 | hash : String,
      | ^
    '',
  exitcode = 1
}