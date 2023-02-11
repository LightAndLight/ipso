{
  description = "https://github.com/LightAndLight/ipso/issues/144",
  args = ["issue144.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue144.ipso:11:1: error: expected one of: '"', '$', '${"\${"}', string
       |
    11 | 
       | ^
    '',
  exitcode = 1
}