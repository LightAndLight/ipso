{
  description = "ipso";
  inputs = {
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cargo2nix = {
      url = "github:cargo2nix/cargo2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        rust-overlay.follows = "rust-overlay";
      };
    };
  };
  outputs = { self, nixpkgs, cargo2nix, rust-overlay }: 
    let 
      pkgs = import nixpkgs { 
        system = "x86_64-linux"; 
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
      
      packages.x86_64-linux = {
        ipso-cli = (rustPkgs { release = true; }).workspace.ipso-cli {};
        ipso-golden = import ./tests/golden { inherit pkgs; };
        ipso-shebang = import ./tests/shebang { inherit pkgs; };
      };
      
      defaultPackage.x86_64-linux = packages.x86_64-linux.ipso-cli;
      
      devShell.x86_64-linux =
        pkgs.mkShell {
          buildInputs = [
            cargo2nix.defaultPackage.x86_64-linux
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
      
      devShells.x86_64-linux.tests =
        let
          RUST_BACKTRACE = false;
        in
          pkgs.mkShell {
            RUST_BACKTRACE = if RUST_BACKTRACE then "1" else "0";
            buildInputs = [
              packages.x86_64-linux.ipso-cli
              packages.x86_64-linux.ipso-golden
              packages.x86_64-linux.ipso-shebang
            ];
          };
    
    };
}
