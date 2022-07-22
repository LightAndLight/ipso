{ mkDerivation, nix-filter, base, filepath, hakyll, lib }:
mkDerivation {
  pname = "generator";
  version = "0.1.0.0";
  src = nix-filter {
    root = ./.;
    include = [
      "generator.cabal"
      "generator.hs"
    ];
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base filepath hakyll ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
