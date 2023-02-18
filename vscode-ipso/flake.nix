{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
        npmPackages = pkgs.symlinkJoin {
          name = "npmPackages";
          paths = [
            pkgs.nodePackages.yo
            pkgs.nodePackages.generator-code 
          ];
        };
      in {
        devShell = pkgs.mkShell {
          NODE_PATH = "${npmPackages}/lib/node_modules";
          buildInputs = with pkgs; [
            npmPackages
            
            nodePackages.npm
            vsce
          ];
        };
      }
    );
}
