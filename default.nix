{ 
  rust-overlaySrc ? builtins.fetchTarball {
    url = 
      let
        rev = "1b723f746e48ea9500007d17c41ad19441b88276";
      in
        "https://github.com/oxalica/rust-overlay/archive/${rev}.tar.gz";
    sha256 = "1gx1528zp3sj9dk53szmi1pab8jjqmlmrbxygbx9ak33bq9nsiv1";
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
      buildInputs = [
        cargo2nix
        pkgs.rust-bin.stable.latest.default
      ];
    };
    dev = mkDerivation { release = false; };
    release = mkDerivation { release = true; };
  }