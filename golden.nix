{ pkgs ? import <nixos-unstable> {}, RUST_BACKTRACE ? false, only ? [] }:
let
  ipso = import ./. { inherit pkgs; };
  ipso-golden = import ./golden { inherit pkgs; };
in
  pkgs.stdenv.mkDerivation {
    name = "ipso-golden-tests";
    src = pkgs.lib.cleanSourceWith {
      filter = path: type: builtins.all (x: x) [
        (builtins.baseNameOf path != ".git")
        (builtins.baseNameOf path != ".gitignore")
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

    RUST_BACKTRACE = if RUST_BACKTRACE then "1" else "0";
    checkPhase = ''
      ipso-golden \
        --bin ${ipso}/bin/ipso \
        --dir examples ${
          if only == []
          then ""
          else ''\
            --only ${pkgs.lib.concatStringsSep "," (map toString only)}
          ''
        }
    '';

    installPhase = ''
      echo "success" > $out
    '';
  }
