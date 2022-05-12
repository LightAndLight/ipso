{
  description = "testing instance DebugRecordFields a => Debug { a }",
  args = ["debugRecordFields.ipso"],
  stdin = None Text,
  stdout =
    ''
    { switch = true }
    true
    true
    true
    true
    { s = 1, t = 2, r = 4 }
    { s = 1, t = 2, r = 4 }
    { s = 1, t = 2, a = 3, r = 4 }
    { s = 1, t = 2, r = 4, a = 3 }
    '',
  stderr = "",
  exitcode = 0
}