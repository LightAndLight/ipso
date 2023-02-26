{
  description = "https://github.com/LightAndLight/ipso/issues/365 test 3",
  args = ["issue365-3.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue365-3.ipso:3:1: error: expected one of: indent (> 0)
      |
    3 | (| 
      | ^
    '',
  exitcode = 1
}