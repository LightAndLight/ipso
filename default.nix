{ pkgs ? import <nixos-unstable> {} }:
pkgs.rustPlatform.buildRustPackage {
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

  cargoSha256 = "135bmkfv1pr2vznz21dmc2rwq3jj9n5q6n3kd1spyj4414r3arj7";
}
