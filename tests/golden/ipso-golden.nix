{ mkDerivation, base, dhall, ipso-tests-common, lib
, optparse-applicative, process, selective, text, vector
}:
mkDerivation {
  pname = "ipso-golden";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base dhall ipso-tests-common optparse-applicative process selective
    text vector
  ];
  license = "unknown";
  mainProgram = "ipso-golden";
}
