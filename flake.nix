{
  description = "Mensam";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs }: {

    subflakes =
      let
        importSubflake = path: inputs: subflakeInputs:
          import path (inputs // { self = (if self ? rev then { inherit (self) rev; } else { }) // { subflakes = subflakeInputs; }; });
      in rec {
          setup = importSubflake ./setup/subflake.nix { } { };
          server = importSubflake ./server/subflake.nix { inherit nixpkgs; } { inherit setup; };
          frontend = importSubflake ./frontend/subflake.nix { inherit nixpkgs; } { inherit setup; };
          static = importSubflake ./static/subflake.nix { inherit nixpkgs; } { inherit setup frontend; };
          final = importSubflake ./final/subflake.nix { inherit nixpkgs; } { inherit setup server static; };
        };

    packages.x86_64-linux.default = self.subflakes.final.packages.x86_64-linux.default;

    overlays.default = self.subflakes.final.overlays.default;

    nixosModules.default = self.subflakes.final.nixosModules.default;

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      let
        additionalBuildInputs = [
          pkgs.asciidoctor
          pkgs.openapi-generator-cli
          pkgs.haskell.packages.ghc926.nix-tree
          pkgs.rnix-lsp
        ];
        shells =
          let
            hasDevShells = name: value: __elem "devShells" (__attrNames value) && __elem "x86_64-linux" (__attrNames value.devShells);
            subflakesWithDevShells = lib.filterAttrs hasDevShells self.subflakes;
            devShellsBySubflake = __mapAttrs (name: value: value.devShells.x86_64-linux) subflakesWithDevShells;
            devShells = __foldl' (a: b: a ++ b) [ ] (map __attrValues (__attrValues devShellsBySubflake));
          in devShells;
        fullBuildInputs = __concatMap (x: x.buildInputs) shells;
        fullNativeBuildInputs = __concatMap (x: x.nativeBuildInputs) shells;
        fullShellHook = __concatStringsSep "\n" (map (x: x.shellHook) shells);
      in stdenv.mkDerivation {
        name = "mensam-development"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        installPhase = "touch $out";
        buildInputs = fullBuildInputs ++ additionalBuildInputs;
        nativeBuildInputs = fullNativeBuildInputs;
        shellHook = fullShellHook;
      };

    checks.x86_64-linux.subflakes =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      pkgs.mkShell {
        packages =
          let
            hasChecks = name: value: __elem "checks" (__attrNames value) && __elem "x86_64-linux" (__attrNames value.checks);
            checkableSubflakes = lib.filterAttrs hasChecks self.subflakes;
            checksBySubflake = __mapAttrs (name: value: value.checks.x86_64-linux) checkableSubflakes;
            checks = __foldl' (a: b: a ++ b) [ ] (map __attrValues (__attrValues checksBySubflake));
          in checks;
      };

  };
}
