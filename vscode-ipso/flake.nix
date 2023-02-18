{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
        npmShell = pkgs.symlinkJoin { name = "npm"; paths = [ pkgs.nodePackages.yo pkgs.nodePackages.generator-code ]; };
      in {
        devShell = pkgs.mkShell {
          NODE_PATH = "${npmShell}/lib/node_modules";
          buildInputs = with pkgs; [
            # nodejs
            npmShell
          ];
        };
      }
    );
}
