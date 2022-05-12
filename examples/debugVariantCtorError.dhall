{
  description = "missing DebugVariantCtor constraint error",
  args = ["debugVariantCtorError.ipso"],
  stdin = None Text,
  stdout = "",
  stderr =
    ''
    debugVariantCtorError.ipso:2:5: error: cannot deduce "Debug ((| A : Int, B : Int, x |))"
      |
    2 | f = debug
      |     ^
    couldn't find instance for "DebugVariantCtor x"
    '',
  exitcode = 1
}