{
  description = "use an imported value in a way that causes a type error",
  args = ["28/main.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    28/main.ipso:4:14: error: expected type "String", got type "Int"
      |
    4 | main = print greeting.value
      |              ^
    '',
  exitcode = 1
}
