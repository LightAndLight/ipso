{
  description = "https://github.com/LightAndLight/ipso/issues/365 test 4",
  args = ["issue365-4.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue365-4.ipso:16:1: error: expected one of: indent (> 0)
       |
    16 | |)
       | ^
    '',
  exitcode = 1
}