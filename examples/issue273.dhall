{
  description = "https://github.com/LightAndLight/ipso/issues/273",
  args = ["issue273.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue273.ipso:3:8: error: expected type "()", got type "r"
      |
    3 |   case x of
      |        ^
    '',
  exitcode = 1
}