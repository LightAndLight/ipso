{
  args = ["20.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    20.ipso:7:19: error: cannot deduce "A (Array a)"
      |
    7 | instance (B a) => B (Array a) where
      |                   ^
    '',
  exitcode = 1
}