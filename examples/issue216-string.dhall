{
  description = "https://github.com/LightAndLight/ipso/issues/216",
  args = ["issue216-string.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    let dollarCurly = "\${" in
    ''
    issue216-string.ipso:2:22: error: expected one of: '"', '$', '${dollarCurly}', string
      |
    2 | main = println "hello
      |                      ^
    '',
  exitcode = 1
}