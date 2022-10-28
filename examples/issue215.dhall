{
  description = "https://github.com/LightAndLight/ipso/issues/215",
  args = ["issue215.ipso"],
  stdin = None Text,
  stdout = "cat dir/file\ncat dir / file\n",
  stderr = "",
  exitcode = 0
}