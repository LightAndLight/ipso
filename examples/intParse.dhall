{
  description = "integer parsing",
  args = ["intParse.ipso"],
  stdin = None Text,
  stdout =
    ''
    Some 11
    Some -11
    Some 11
    Some -11
    None ()
    Some 83
    Some -83
    Some 83
    Some -83
    None ()
    Some 1011
    Some -1011
    None ()
    Some 26
    Some 26
    Some -8
    Some 26
    Some -26
    '',
  stderr = "",
  exitcode = 0
}