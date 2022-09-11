{
  description = "https://github.com/LightAndLight/ipso/issues/218",
  args = ["issue218.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue218.ipso:5:8: error: expected type "Bool", got type "String"
      |
    5 |     if string then io.pure () else io.pure ()
      |        ^
    '',
  exitcode = 1
}