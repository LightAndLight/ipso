{
  description = "() pattern failure",
  args = ["unitPattern2.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    unitPattern2.ipso:4:5: error: expected type "Int", got type "()"
      |
    4 |     () -> println "hi"
      |     ^
    '',
  exitcode = 1
}
