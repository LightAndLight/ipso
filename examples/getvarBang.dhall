{
  description = "env.getvar! failure",
  args = ["getvarBang.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = "getvar!: missing environment variable THIS_IS_NOT_SET",
  exitcode = 1
}