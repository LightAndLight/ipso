{
  rust-overlaySrc ? builtins.fetchTarball {
    url =
      let
        rev = "50d8019b4d68420471ebb012de175ea450f55c44";
      in
        "https://github.com/oxalica/rust-overlay/archive/${rev}.tar.gz";
    sha256 = "0w808jbvvcnq94a4ybl1b47v2hd23hf2b97kdh6qnbyvmj4382vr";
  },
  cargo2nixSrc ? builtins.fetchTarball {
    url =
      let
        rev = "92103dc68ecf4f386c818321e19ae964c20b88d7";
      in
        "https://github.com/cargo2nix/cargo2nix/archive/${rev}.tar.gz";
    sha256 = "1bq11yyskfrf0w243ig19j7ds1rna54lsxpp3c441ls8j4zkxlnq";
  },
  overlays ? [],
}:
let
  nixpkgs = <nixos-unstable>;
  pkgs = import nixpkgs {
    overlays = overlays ++ [
      (import "${cargo2nixSrc}/overlay")
      (import rust-overlaySrc)
    ];
  };

  cargo2nix = (import cargo2nixSrc {
    inherit nixpkgs;
    rust-overlay = rust-overlaySrc;
  }).package;

  rustPkgs = args: pkgs.rustBuilder.makePackageSet' {
    rustChannel = "stable";
    packageFun = import ./Cargo.nix;
    inherit (args) release;
  };

  ipso = rustPkgsArgs: args: (rustPkgs rustPkgsArgs).workspace.ipso args;
  ipsoTests = pkgs.rustBuilder.runTests (ipso { release = false; }) {};

  mkDerivation = rustPkgsArgs:
    pkgs.stdenv.mkDerivation {
      name = "ipso";
      unpackPhase = "true";

      buildPhase = "true";

      doCheck = true;
      checkInputs = [
        ipsoTests
      ];
      checkPhase = "true";

      installPhase = ''
        mkdir -p $out
        cp -R ${(ipso rustPkgsArgs {}).bin}/* $out/
      '';
    };
in
  {
    inherit pkgs;
    shell = pkgs.mkShell {
      buildInputs = with pkgs; [
        cargo2nix
        (rust-bin.nightly.latest.default.override {
          extensions = [
            "cargo"
            "rustc"
            "rust-src"
            "rustfmt"
          ];
        })
      ];
    };
    dev = mkDerivation { release = false; };
    release = mkDerivation { release = true; };
  }
