{
  inputs = {
    # Note: replace with upstream url when PR
    # https://github.com/nammayatri/common/pull/13 gets merged
    # common.url = "github:nammayatri/common";
    common.url = "github:arjunkathuria/common/Mobility-GHC927-Rebased";
    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    systems.url = "github:nix-systems/default";

    passetto-hs.url = "github:juspay/passetto/bb92cf1dd9699662d2a7bb96cd6a6aed6f20e8ff";
    passetto-hs.flake = false;

    # replace this with upstream when done
    clickhouse-haskell.url = "github:arjunkathuria/clickhouse-haskell/Mobility\/GHC-927-rebased";
    clickhouse-haskell.inputs.common.follows = "common";

    # Note: Prometheus-haskell now comes transitively from euler-events-hs
    # which is a dependency of euler-hs flake
    # prometheus-haskell.url = "github:juspay/prometheus-haskell/more-proc-metrics";
    euler-hs.url = "github:arjunkathuria/euler-hs/Mobility-GHC927";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      debug = true;
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.euler-hs.haskellFlakeProjectModules.output
            inputs.clickhouse-haskell.haskellFlakeProjectModules.output
            # inputs.prometheus-haskell.haskellFlakeProjectModules.output
          ];
          packages = {
            passetto-client.source = inputs.passetto-hs + /client;
            passetto-core.source = inputs.passetto-hs + /core;
          };
          settings = {
            # Tests and documentation generation fail for some reason.
            euler-hs = {
              check = false;
              haddock = false;
            };
            haxl = {
              broken = false;
              jailbreak = true;
            };
            wai-middleware-prometheus = {
              check = false;
              haddock = false;
            };
            clickhouse-haskell.jailbreak = true;
            openapi3 = {
              broken = false;
              check = false;
            };
          };
          autoWire = [ "packages" "checks" ];
        };
        packages.default = self'.packages.mobility-core;
        devShells.default = pkgs.mkShell {
          # cf. https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.pre-commit.devShell
            config.flake-root.devShell
          ];
        };
      };
    };
}
