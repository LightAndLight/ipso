{
  description = "demonstrating IO referential transparency by using stdin/stdout",
  args = ["12.ipso"],
  stdin = Some "hello\ngoodbye\n",
  stdout = "hello\ngoodbye\n",
  stderr = "",
  exitcode = 0
}