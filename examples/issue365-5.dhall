{
  description = "https://github.com/LightAndLight/ipso/issues/365 test 5",
  args = ["issue365-5.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue365-5.ipso:7:1: error: expected one of: indent (> 0)
      |
    7 | { 
      | ^
    '',
  exitcode = 1
}