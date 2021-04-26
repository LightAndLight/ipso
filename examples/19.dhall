{
  description = "duplicate type class argument error",
  args = ["19.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    19.ipso:1:11: error: duplicate type class argument
      |
    1 | class A b b where
      |           ^
    '',
  exitcode = 1
}