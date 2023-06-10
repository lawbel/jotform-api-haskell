{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [haskell-flake.flakeModule];
      perSystem = { self', pkgs, ... }: {
        packages.default = self'.packages.jotform-api-haskell;
        haskellProjects.default = {
          devShell = {
            tools = haskell: {
              fourmolu = haskell.fourmolu;
            };
          };
        };
      };
    };
}
