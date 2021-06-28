let concatSep = https://prelude.dhall-lang.org/Text/concatSep.dhall
in {
  description = "todolist test 2",
  args = ["todolist.ipso"],
  stdin = Some (concatSep "\n" [
    "help",
    "add entry1", 
    "list",
    "add entry2", 
    "list", 
    "add entry3", 
    "list", 
    "delete 1", 
    "list", 
    "quit\n"
  ]),
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
