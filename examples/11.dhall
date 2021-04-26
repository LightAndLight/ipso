{
  description = "echo stdin to stdout twice",
  args = ["11.ipso"],
  stdin = Some "hello\ngoodbye\n",
  stdout = "hello\ngoodbye\n",
  stderr = "",
  exitcode = 0
}