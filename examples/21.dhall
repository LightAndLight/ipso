{
  description = "unsatifed assumption during instance declaration",
  args = ["21.ipso"],
  stdin = None Text,
  stdout = "",
  stderr = 
    ''
    21.ipso:10:10: error: cannot deduce "A (Array a)"
       |
    10 | instance B (Array a) where
       |          ^
    '',
  exitcode = 1
}