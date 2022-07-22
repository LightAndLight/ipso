{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };
  outputs = { self, nixpkgs, flake-utils, nix-filter }: 
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; }; in {
        devShell =
          pkgs.mkShell {
            buildInputs = [
              pkgs.cabal2nix
              pkgs.haskellPackages.cabal-install
              pkgs.haskellPackages.haskell-language-server
              pkgs.zlib
            ];
          };
        packages = rec {
          generator = pkgs.haskellPackages.callPackage ./generator.nix { nix-filter = nix-filter.lib; };
          
          site = pkgs.stdenv.mkDerivation {
            name = "ipso-site";
            
            src = let filter = nix-filter.lib; in filter {
              root = ./.;
              include = [
                (filter.inDirectory "css")
                (filter.inDirectory "images")
                (filter.inDirectory "pages")
                (filter.inDirectory "templates")
              ];
            };
            
            buildInputs = with pkgs; [
              generator
            ];
            
            LANG = "en_US.UTF-8";
            LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            buildPhase = ''
              generator build
            '';
            
            installPhase = ''
              mv _site $out
            '';
          };
        };
      }
    );
}
