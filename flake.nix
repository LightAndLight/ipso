{
  description = "ipso";
  inputs = {
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    cargo2nix = {
      url = "github:cargo2nix/cargo2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        rust-overlay.follows = "rust-overlay";
      };
    };
  };
  outputs = { self, nixpkgs, flake-utils, cargo2nix, rust-overlay }: 
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let 
        pkgs = import nixpkgs { 
          inherit system; 
          overlays = [ 
            (import "${cargo2nix}/overlay")
            rust-overlay.overlay 
          ]; 
        };
        
        rustPkgs = { release }: pkgs.rustBuilder.makePackageSet' {
          rustChannel = "1.56.1";
          packageFun = import "${self}/Cargo.nix";
          inherit release;
        };
      in rec {
        packages = {
          ipso-cli = (rustPkgs { release = true; }).workspace.ipso-cli {};
          ipso-golden = import ./tests/golden { inherit pkgs; };
          ipso-shebang = import ./tests/shebang { inherit pkgs; };
        };
        defaultPackage = packages.ipso-cli;
        devShell =
          pkgs.mkShell {
            buildInputs = [
              cargo2nix.defaultPackage.${system}
              (pkgs.rust-bin.stable."1.56.1".default.override {
                extensions = [
                  "cargo"
                  "clippy"
                  "rustc"
                  "rust-src"
                  "rustfmt"
                ];
              })
            ];
          };
      devShells.tests =
        let
          RUST_BACKTRACE = false;
        in
          pkgs.mkShell {
            RUST_BACKTRACE = if RUST_BACKTRACE then "1" else "0";
            buildInputs = [
              packages.ipso-cli
              packages.ipso-golden
              packages.ipso-shebang
            ];
          };
      }
    );
}