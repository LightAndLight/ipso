{
  description = "read two strings from stdout, concatenate, then write to stdout",
  args = ["10.ipso"],
  stdin = Some "hello\ngoodbye\n",
  stdout = "hello\ngoodbye\n",
  stderr = "",
  exitcode = 0
}