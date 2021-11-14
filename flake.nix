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
    in {
      defaultPackage.x86_64-linux =
        let  
          rustPkgs = args: pkgs.rustBuilder.makePackageSet' {
            rustChannel = "1.56.1";
            packageFun = import "${self}/Cargo.nix";
            inherit (args) release;
          };

          ipso = rustPkgsArgs: args: (rustPkgs rustPkgsArgs).workspace.ipso-cli args;
          ipsoTests = pkgs.rustBuilder.runTests (ipso { release = false; }) {};
        in
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
              cp -R ${ipso { release = true; } {}}/* $out/
            '';
          };
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
    };
}
