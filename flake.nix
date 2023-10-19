{
  description = "e1-268H's basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { flake-utils, nixpkgs, self, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {};
        overlays = [];
        pkgs = import nixpkgs { inherit config overlays system; };
        haskellPackages = pkgs.haskellPackages;
        packageName = "e1-268H";
      in rec {
        packages.${packageName} =
          pkgs.haskell.lib.dontCheck (
            haskellPackages.callCabal2nix packageName self rec {
              # Dependency overrides go here
            }
          );

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          inputsFrom = builtins.attrValues self.packages.${system};
          withHoogle = true;
          packages = p: [];
          buildInputs = with haskellPackages; [
            # Package Managers
            cabal-install
            stack

            # Extra dev tools
            haskell-language-server
            hlint
            ormolu
          ];
        };
      }
    );
}
