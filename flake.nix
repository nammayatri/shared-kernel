{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    common.url = "github:nammayatri/common";
    flake-parts.follows = "common/flake-parts";

    passetto-hs.url = "github:juspay/passetto/bb92cf1dd9699662d2a7bb96cd6a6aed6f20e8ff";
    passetto-hs.flake = false;

    # euler-hs and its transitive dependencies
    euler-hs.url = "github:srid/euler-hs/ghc810--nixify";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.common.flakeModules.default
      ];

      perSystem = { self', pkgs, lib, config, ... }: {
        cachix-push.packages = [ "default" ];
        packages.default = self'.packages.mobility-core;
        haskellProjects.default = {
          imports = [
            inputs.euler-hs.haskellFlakeProjectModules.output
          ];
          source-overrides = {
            passetto-client = inputs.passetto-hs + /client;
            passetto-core = inputs.passetto-hs + /core;
          };
          overrides = self: super:
            with pkgs.haskell.lib.compose;
            lib.mapAttrs (k: v: lib.pipe super.${k} v) {
              # Tests and documentation generation fail for some reason.
              euler-hs = [ dontCheck dontHaddock ];
            };
          basePackages = config.haskellProjects.ghc810.outputs.finalPackages;
          packages.mobility-core.root = ./lib/mobility-core;
        };
      };
    });
}
