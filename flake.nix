{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        packages.default = self'.packages.jotform-api-haskell;
        haskellProjects.default = {
          basePackages = pkgs.haskellPackages;
          devShell = {
            enable = true;
            tools = haskell: {
              fourmolu = haskell.fourmolu;
            };
            hlsCheck.enable = true;
          };
        };
      };
    };
}
