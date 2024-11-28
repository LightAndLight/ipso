{
  description = "ipso";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    cargo2nix.url = "github:cargo2nix/cargo2nix";
  };
  outputs = { self, nixpkgs, flake-utils, cargo2nix, rust-overlay }:
    let
      systemTargets = {
        "x86_64-linux" = "x86_64-unknown-linux-musl";
        "x86_64-darwin" = "x86_64-apple-darwin";
      };
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            cargo2nix.overlays.default
            rust-overlay.overlays.default
          ];
        };

        rustVersion = "1.69.0";
        
        rustPkgs = { release }: pkgs.rustBuilder.makePackageSet {
          rustChannel = rustVersion;
          packageFun = import "${self}/Cargo.nix";
          inherit release;
          target = systemTargets.${system};
        };
      in rec {
        packages = {
          ipso-cli = (rustPkgs { release = true; }).workspace.ipso-cli {};
          ipso-golden = pkgs.haskell.lib.justStaticExecutables (import ./tests/golden { inherit pkgs; });
          ipso-shebang = pkgs.haskell.lib.justStaticExecutables (import ./tests/shebang { inherit pkgs; });
          test-package = pkgs.stdenv.mkDerivation {
            name = "test-package";
            unpackPhase = "true";
            buildPhase = "true";
            installPhase = ''
              mkdir $out
              echo "test 4" > $out/test
            '';
          };
        };

        defaultPackage = packages.ipso-cli;

        devShell =
          pkgs.mkShell {
            buildInputs = [
              cargo2nix.packages.${system}.cargo2nix
              (pkgs.rust-bin.stable.${rustVersion}.default.override {
                extensions = [
                  "cargo"
                  "clippy"
                  "rustc"
                  "rust-src"
                  "rustfmt"
                  "rust-analyzer"
                ];
              })

              # for running tests locally
              pkgs.cabal2nix
              packages.ipso-golden
              packages.ipso-shebang

              # profiling
              pkgs.kcachegrind
              pkgs.valgrind
            ];
          };

        devShells.tests =
          (rustPkgs { release = false; }).workspaceShell {
            buildInputs = [
              packages.ipso-golden
              packages.ipso-shebang
            ];
          };
      }
    );
}
