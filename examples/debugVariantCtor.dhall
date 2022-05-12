{
  description = "testing instance DebugVariantCtor a => Debug (| a |)",
  args = ["debugVariantCtor.ipso"],
  stdin = None Text,
  stdout =
    ''
    Identity 1
    S 1
    T 2
    A 3
    R 4
    S 1
    skipped
    A 3
    R 4
    S 1
    T 2
    skipped
    R 4
    S 1
    T 2
    A 3
    R 4
    '',
  stderr = "",
  exitcode = 0
}