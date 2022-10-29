{
  description = "https://github.com/LightAndLight/ipso/issues/216",
  args = ["issue216-cmd.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue216-cmd.ipso:2:21: error: expected one of: '$', '`', command fragment, space
      |
    2 | main = cmd.run `echo
      |                     ^
    '',
  exitcode = 1
}