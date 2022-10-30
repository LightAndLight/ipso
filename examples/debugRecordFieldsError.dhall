{
  description = "missing DebugRecordFields constraint error",
  args = ["debugRecordFieldsError.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    debugRecordFieldsError.ipso:2:5: error: cannot deduce "Debug ({ s : Int, t : Int, x })"
      |
    2 | f = debug
      |     ^
    couldn't find instance for "DebugRecordFields x"
    '',
  exitcode = 1
}