{ mkDerivation, base, dhall, directory, filepath, lib
, optparse-applicative, process, selective, temporary, text
}:
mkDerivation {
  pname = "ipso-tests-common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base dhall directory filepath optparse-applicative process
    selective temporary text
  ];
  license = "unknown";
}
