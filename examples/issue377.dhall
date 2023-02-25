{
  description = "https://github.com/LightAndLight/ipso/issues/377",
  args = ["issue377.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    issue377.ipso:6:20: error: expected type "(| Some : a, None : () |)", got type "a"
      |
    6 |         None () -> item
      |                    ^
    '',
  exitcode = 1
}