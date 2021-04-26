{
  description = "echo one line of stdin to stdout",
  args = ["9.ipso"],
  stdin = Some "hello\ngoodbye",
  stdout = "hello\n",
  stderr = "",
  exitcode = 0
}