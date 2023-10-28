{
  description = "Wordlelm";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem
    (system:
      let
        packageName = "wordlelm";
        pkgs = nixpkgs.legacyPackages.${system};
      in
      rec {
        packages.${packageName} = pkgs.python3Packages.buildPythonApplication rec {
          name = packageName;
          src = ./src;
          buildInputs = [
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-test
            pkgs.elmPackages.elm-language-server
          ];
        };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-test
            pkgs.elmPackages.elm-language-server
          ];
        };
      });

}
