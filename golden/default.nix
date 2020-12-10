{ pkgs ? import <nixpkgs> {} }:
let 
  src = pkgs.lib.cleanSourceWith {
    filter = path: type: builtins.all (x: x) [
      (builtins.baseNameOf path != "default.nix")
      (builtins.baseNameOf path != "shell.nix")
      (builtins.baseNameOf path != "dist-newstyle")
    ];
    src = ./.;
  };
in
pkgs.haskellPackages.callCabal2nix "ipso-golden" src {}
