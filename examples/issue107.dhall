{
  description = "https://github.com/LightAndLight/ipso/issues/107",
  args = ["issue107.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue107.ipso:1:15: error: type not in scope
      |
    1 | test : Int -> Test
      |               ^
    '',
  exitcode = 1
}