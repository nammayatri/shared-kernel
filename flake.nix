{
  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    haskell-flake.follows = "common/haskell-flake";

    passetto-hs.url = "github:juspay/passetto/bb92cf1dd9699662d2a7bb96cd6a6aed6f20e8ff";
    passetto-hs.flake = false;

    clickhouse-haskell.url = "github:nammayatri/clickhouse-haskell";
    clickhouse-haskell.inputs.common.follows = "common";
    prometheus-haskell.url = "github:juspay/prometheus-haskell/more-proc-metrics";
    prometheus-haskell.inputs.haskell-flake.follows = "common/haskell-flake";

    # Amazonka 2.0 tagged release.
    # The nixpkgs amazonka-2.0 package is marked broken, so it is built from source (same as the backend).
    amazonka-git.url = "github:brendanhay/amazonka?ref=2.0.0";
    amazonka-git.flake = false;

    euler-hs = {
      url = "github:nammayatri/euler-hs";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.haskell-flake.follows = "haskell-flake";
    };
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
            amazonka.source = inputs.amazonka-git + /lib/amazonka;
            amazonka-core.source = inputs.amazonka-git + /lib/amazonka-core;
            amazonka-test.source = inputs.amazonka-git + /lib/amazonka-test;
            amazonka-sso.source = inputs.amazonka-git + /lib/services/amazonka-sso;
            amazonka-sts.source = inputs.amazonka-git + /lib/services/amazonka-sts;
            amazonka-kms.source = inputs.amazonka-git + /lib/services/amazonka-kms;
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
            amazonka-core.check = false;
            generic-deriving.check = false;
            singletons-th.jailbreak = true;
          };
          autoWire = [ "packages" "checks" ];
        };
        process-compose = { };
        packages.default = self'.packages.mobility-core;
        devShells.default = pkgs.mkShell {
          name = "shared-kernel-shell";
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
