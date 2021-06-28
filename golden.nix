{ pkgs ? import <nixos-unstable> {}, RUST_BACKTRACE ? false }:
let
  ipso = import ./. { inherit pkgs; };
  ipso-golden = import ./golden { inherit pkgs; };
in
  pkgs.mkShell {
    RUST_BACKTRACE = if RUST_BACKTRACE then "1" else "0";
    buildInputs = [
      ipso
      ipso-golden
    ];
  }
