let
  pkgs = import <nixos-unstable> {};
in
pkgs.stdenv.mkDerivation {
  name = "ipso";
  src = ./src;
  buildInputs = with pkgs; [
    cargo
  ];
}
