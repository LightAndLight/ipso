{ debug ? false, RUST_BACKTRACE ? false }:
let
  ipso = import ./. {};
  pkgs = ipso.pkgs;
  ipso-golden = import ./golden { inherit pkgs; };
in
  pkgs.mkShell {
    RUST_BACKTRACE = if RUST_BACKTRACE then "1" else "0";
    buildInputs = [
      (if debug then ipso.dev else ipso.release)
      ipso-golden
    ];
  }
