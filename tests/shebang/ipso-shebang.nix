{ mkDerivation, base, dhall, filepath, ipso-tests-common, lib
, process, selective, text
}:
mkDerivation {
  pname = "ipso-shebang";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base dhall filepath ipso-tests-common process selective text
  ];
  license = "unknown";
  mainProgram = "ipso-shebang";
}
