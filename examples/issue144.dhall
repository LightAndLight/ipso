{
  description = "https://github.com/LightAndLight/ipso/issues/144",
  args = ["issue144.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue144.ipso:7:53: error: expected one of: '"', '$', '${"\${"}', string
      |
    7 |     println <| showBool (toArgs "hello" == ["hello])
      |                                                     ^
    '',
  exitcode = 1
}