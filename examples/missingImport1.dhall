{
  description = "missing import error message",
  args = ["missingImport1.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    missingImport1.ipso:1:8: error: module not found
      |
    1 | import missing_module
      |        ^
    file missing_module.ipso does not exist
    '',
  exitcode = 1
}