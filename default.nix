{ pkgs ? import <nixos-unstable> {} }:
pkgs.rustPlatform.buildRustPackage {
  name = "ipso";
  src = pkgs.lib.cleanSourceWith {
    filter = path: type: builtins.all (x: x) [
      (builtins.baseNameOf path != "default.nix")
      (builtins.baseNameOf path != "REFERENCE.md")
      (builtins.baseNameOf path != "target")
      (builtins.baseNameOf path != "golden")
      (builtins.baseNameOf path != "golden.nix")
      (builtins.baseNameOf path != "examples")
    ];
    src = ./.;
  };

  cargoSha256 = "1f330yjqvnf2srlp3br2dbz8hmkfdm2a922pfs4ykj57ikhxfxpi";
}
