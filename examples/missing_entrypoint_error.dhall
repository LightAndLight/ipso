{
  description = "error message when entrypoint is missing",
  args = ["missing_entrypoint_error.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    missing_entrypoint_error.ipso: error: missing entrypoint "main"
    '',
  exitcode = 1
}