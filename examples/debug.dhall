{
  description = "testing debug instances",
  args = ["debug.ipso"],
  stdin = None Text,
  stdout =
    ''
    1
    true
    [1, 2, 3]
    { z = 1, y = 2, x = 3 }
    Never 2
    'x'
    "hello\nworld\n"
    ``
    `echo`
    `echo hello`
    `echo hello world`
    `echo "hello world"`
    `echo hello \` world`
    `echo "hello ` world"`
    '',
  stderr = "",
  exitcode = 0
}