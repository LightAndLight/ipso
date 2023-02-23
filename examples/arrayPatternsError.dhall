{
  description = "array pattern matching type error",
  args = ["arrayPatternsError.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    arrayPatternsError.ipso:2:5: error: expected type "Int", got type "Array ?3"
      |
    2 | one [a, b, c] = "$a$b$c"
      |     ^
    '',
  exitcode = 1
}