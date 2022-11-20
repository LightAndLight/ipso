{
  description = "nested patterns",
  args = ["nestedPatterns.ipso"],
  stdin = None Text,
  stdout =
    ''
    99
    Ok 88
    Err "error 1"
    Err "error 2"
    A 1
    A 2
    A 3
    A 4
    A _
    B _
    B _
    B 'c'
    B 'd'
    B _
    C { x = 1, y = 2 }
    C { x = 3, y = 4 }
    '',
  stderr = "",
  exitcode = 0
}