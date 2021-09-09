{ pkgs ? import <nixos-unstable> {}, debug ? false }:
pkgs.rustPlatform.buildRustPackage ({
  name = "ipso";
  src = pkgs.lib.cleanSourceWith {
    filter = path: type: builtins.all (x: x) [
      (builtins.baseNameOf path != ".git")
      (builtins.baseNameOf path != ".gitignore")
      (builtins.baseNameOf path != "default.nix")
      (builtins.baseNameOf path != "REFERENCE.md")
      (builtins.baseNameOf path != "target")
      (builtins.baseNameOf path != "golden")
      (builtins.baseNameOf path != "golden.nix")
      (builtins.baseNameOf path != "examples")
    ];
    src = ./.;
  };
  
  checkPhase = ''
    cargo test
  '';

  cargoSha256 = "0jsvgq55ixym3q80yl0666c9bl10j34m7sbmanl8dg84qs2f2gx2";
} // (if debug then { buildType = "debug"; } else {})
)
