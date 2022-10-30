{
  description = "duplicate top level definitions are not allowed",
  args = ["duplicateTopLevelDefs.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    duplicateTopLevelDefs.ipso:5:1: error: already defined
      |
    5 | a = "hi"
      | ^
    '',
  exitcode = 1
}