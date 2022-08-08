{
  description = "https://github.com/LightAndLight/ipso/issues/144",
  args = ["issue144.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue144.ipso:8:1: error: expected one of: '"', '$', '${"\${"}', string
      |
    8 | 
      | ^
    '',
  exitcode = 1
}