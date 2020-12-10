{ pkgs ? import <nixos-unstable> {} }:
let
  ipso = import ./. { inherit pkgs; };
  ipso-golden = import ./golden { inherit pkgs; };
in
  pkgs.stdenv.mkDerivation {
    name = "ipso-golden-tests";
    src = pkgs.lib.cleanSourceWith {
      filter = path: type: builtins.all (x: x) [
        (builtins.baseNameOf path != "default.nix")
        (builtins.baseNameOf path != "REFERENCE.md")
        (builtins.baseNameOf path != "target")
        (builtins.baseNameOf path != "golden")
        (builtins.baseNameOf path != "src")
      ];
      src = ./.;
    };

    buildPhase = "true";

    doCheck = true;
    checkInputs = [
      ipso-golden
    ];
    checkPhase = ''
      ipso-golden \
        --bin ${ipso}/bin/ipso \
        --dir examples
    '';

    installPhase = ''
      echo "success" > $out
    '';
  }
