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

  cargoSha256 = "1vwhpnnmljdgx4zgfzrifb5lyg7dydg1jz6g55yl747bfs8can39";
} // (if debug then { buildType = "debug"; } else {})
)
