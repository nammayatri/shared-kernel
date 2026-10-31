{
  inputs = {
    common.url = "github:nammayatri/common?ref=ghc-98-upgrade";
    nixpkgs.follows = "common/nixpkgs";
    haskell-flake.follows = "common/haskell-flake";

    passetto-hs.url = "github:nammayatri/passetto/use-crypton";
    passetto-hs.flake = false;

    clickhouse-haskell.url = "github:nammayatri/clickhouse-haskell?ref=ghc-98-upgrade";
    clickhouse-haskell.inputs.common.follows = "common";

    prometheus-haskell.url = "github:nammayatri/prometheus-haskell?ref=ghc-98-upgrade";
    prometheus-haskell.inputs.haskell-flake.follows = "common/haskell-flake";

    euler-hs = {
      url = "github:nammayatri/euler-hs?ref=ghc-98-upgrade";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.haskell-flake.follows = "haskell-flake";
    };

    jwt.url = "github:puffnfresh/haskell-jwt?rev=2b3167c4522e435410337d9c0da4523ead26c345";
    jwt.flake = false;

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
            jwt.source = inputs.jwt;
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
            generic-deriving.check = false;

            jwt = { self, ... }: {
              cabalFlags.use_crypton = true;
              extraBuildDepends = [
                self.crypton
                self.crypton-x509
                self.crypton-x509-store
              ];
              check = false;
            };
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
