{
  description = "todolist test 1",
  args = ["todolist.ipso"],
  stdin = Some "help\nquit\n",
  stdout =
    ''
    Welcome to your TODO list
    > help - display this message
    quit - exit the application
    list - display the todolist
    add STRING - add a task to the list
    delete INT - delete a task from the list
    > '',
  stderr = "",
  exitcode = 0
}
