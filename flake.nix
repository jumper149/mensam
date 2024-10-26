{
  description = "Mensam";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    weeder-nix = {
      type = "github";
      owner = "NorfairKing";
      repo = "weeder-nix";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-github-actions = {
      type = "github";
      owner = "nix-community";
      repo = "nix-github-actions";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, weeder-nix, nix-github-actions }: {

    subflakes =
      let
        importSubflake = path: inputs: subflakeInputs:
          import path (inputs // { self = (if self ? rev then { inherit (self) rev; } else { }) // { subflakes = subflakeInputs; }; });
      in rec {
          setup = importSubflake ./setup/subflake.nix { } { };
          server = importSubflake ./server/subflake.nix { inherit nixpkgs weeder-nix; } { inherit setup; };
          frontend = importSubflake ./frontend/subflake.nix { inherit nixpkgs; } { inherit setup; };
          static = importSubflake ./static/subflake.nix { inherit nixpkgs; } { inherit setup frontend; };
          final = importSubflake ./final/subflake.nix { inherit nixpkgs; } { inherit setup server static; };
        };

    packages.x86_64-linux.default = self.subflakes.final.packages.x86_64-linux.default;

    overlays.default = self.subflakes.final.overlays.default;

    dockerImages.default = self.subflakes.final.dockerImages.default;

    dockerImages.devcontainer =
      let
        source =
          with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
          {
            docker-nixpkgs = pkgs.fetchFromGitHub {
              owner  = "nix-community";
              repo   = "docker-nixpkgs";
              rev    = "bccad7f19ef17f19aa39f23ea9b5b4ad2031b505";
              sha256 = "sha256-CGXnLRVAo0hN62PfLEnNfB6jaTQAupDLS+1ZCHDTPiE=";
            };
          };
        docker-nixpkgs-pkgs = import nixpkgs {
          config = { };
          system = "x86_64-linux";
          overlays = [
            (import "${source.docker-nixpkgs}/overlay.nix")
          ];
        };
      in
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      pkgs.dockerTools.buildImage {
        name = "mensam-devcontainer";
        fromImage = docker-nixpkgs-pkgs.docker-nixpkgs.devcontainer;
        contents = [
          pkgs.cacert
          pkgs.direnv
          (writeTextFile {
            name = "direnv.toml";
            destination = "/root/.config/direnv/direnv.toml";
            text = ''
              [whitelist]
                prefix = [ "/" ]
            '';
          })
          (writeTextFile {
            name = "nix.conf";
            destination = "/etc/nix/nix.conf";
            text = ''
              accept-flake-config = true
              experimental-features = nix-command flakes
              substituters = https://cache.nixos.org/  https://jumper149-mensam.cachix.org
              trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= jumper149-mensam.cachix.org-1:9502wAOm00GdLxZM8uTE4goaBGCpHb+d1jUt3dhR8ZM=
            '';
          })
        ];
      };

    nixosModules.default = self.subflakes.final.nixosModules.default;

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      let
        additionalBuildInputs = [
          pkgs.asciidoctor
          pkgs.curl
          pkgs.jq
          pkgs.openapi-generator-cli
          pkgs.nix-tree
          pkgs.nil
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

    checks.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      let
        hasChecks = name: value: __elem "checks" (__attrNames value) && __elem "x86_64-linux" (__attrNames value.checks);
        checkableSubflakes = lib.filterAttrs hasChecks self.subflakes;
        checksBySubflake =
          __mapAttrs (nameSubflake: valueSubflake:
            lib.mapAttrs' (nameCheck: valueCheck:
              lib.nameValuePair
                ("subflake" + "-" + nameSubflake + "-" + nameCheck)
                valueCheck
            ) valueSubflake.checks.x86_64-linux
          ) checkableSubflakes;
        checks = __foldl' (a: b: a // b) { } (__attrValues checksBySubflake);
      in checks;

    githubActions =
      with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
      let
        generated =
          nix-github-actions.lib.mkGithubMatrix {
            checks = self.checks;
          };
      in {
        checks = generated.checks;
        matrix = {
          include =
            let
              addDeploymentDimension = deployment: matrix:
                matrix // {
                  deployment = deployment;
                };
              includeWithAllDimensions =
                (map (addDeploymentDimension "development") generated.matrix.include) ++
                (map (addDeploymentDimension "nixpublic") generated.matrix.include);
              isBrokenCheck = matrix:
                matrix.deployment == "nixpublic" && matrix.attr == "githubActions.checks.x86_64-linux.\"subflake-final-mensam-test\"";
              removeBrokenChecks = __filter (matrix: ! isBrokenCheck matrix);
            in removeBrokenChecks includeWithAllDimensions;
        };
      };

  };
}
