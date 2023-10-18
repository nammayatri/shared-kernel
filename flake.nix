{
  inputs = {
    common.url = "github:nammayatri/common";

    passetto-hs.url = "github:juspay/passetto/bb92cf1dd9699662d2a7bb96cd6a6aed6f20e8ff";
    passetto-hs.flake = false;

    clickhouse-haskell.url = "github:nammayatri/clickhouse-haskell";
    clickhouse-haskell.inputs.common.follows = "common";
    prometheus-haskell.url = "github:juspay/prometheus-haskell/more-proc-metrics";
    prometheus-haskell.inputs.haskell-flake.follows = "common/haskell-flake";

    euler-hs.url = "github:vijaygupta18/euler-hs/backend/adding-class-for-column-mapping";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      debug = true;
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          imports = [
            inputs.euler-hs.haskellFlakeProjectModules.output
            inputs.clickhouse-haskell.haskellFlakeProjectModules.output
          ];
          packages = {
            passetto-client.source = inputs.passetto-hs + /client;
            passetto-core.source = inputs.passetto-hs + /core;
          };
          settings = {
            # Tests and documentation generation fail for some reason.
            euler-hs = {
              check = false;
              jailbreak = true;
              haddock = false;
              libraryProfiling = false;
            };
            wai-middleware-prometheus = {
              check = false;
              haddock = false;
            };
            euler-events-hs = {
              check = false;
              libraryProfiling = false;
              jailbreak = true;
            };
            prometheus-client = {
              check = false;
              libraryProfiling = false;
              jailbreak = true;
            };
            clickhouse-haskell.jailbreak = true;
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

