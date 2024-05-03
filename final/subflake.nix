{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "mensam-server-full" ''
      MENSAM_CONFIG_FILE="${packages.x86_64-linux.config}" ${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-server
    '';

  packages.x86_64-linux.mensam-test =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "mensam-test-full" ''
      export MENSAM_CONFIG_FILE="${packages.x86_64-linux.config}"
      export MENSAM_LOG_LEVEL=LevelWarn
      ${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-test
    '';

  packages.x86_64-linux.mensam-openapi =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "mensam-openapi-full" ''
      ${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-openapi
    '';

  packages.x86_64-linux.mensam-client =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "mensam-client-full" ''
      ${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-client $@
    '';

  packages.x86_64-linux.config =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeText "mensam.json" (builtins.toJSON config);

  packages.x86_64-linux.fallback =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    self.subflakes.static.packages.x86_64-linux.default;

  deployment = builtins.fromJSON (builtins.readFile ./deployment.json);

  config =
    builtins.fromJSON (builtins.readFile ./configurations/${deployment}.json) // {
      revision = if self ? rev then self.rev else null;
      directory-static = "${self.subflakes.static.packages.x86_64-linux.default}";
    };

  overlays.default = final: prev: {
    mensam = {
      exe = self.subflakes.server.packages.x86_64-linux.default;
      full = {
        server = packages.x86_64-linux.default;
        client = packages.x86_64-linux.mensam-client;
        openapi = packages.x86_64-linux.mensam-openapi;
        fallback = packages.x86_64-linux.fallback;
      };
      config.default = config;
    };
  };

  nixosModules.default = import ./module.nix {
    finalOverlay = overlays.default;
  };

  checks.x86_64-linux.packageDefault = packages.x86_64-linux.default;

  checks.x86_64-linux.mensam-test =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "mensam-test"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        set +e
        TEST_LOG="$(mensam-test-full || echo "Server test exited abnormally.")"
        set -e
        echo "$TEST_LOG"

        if [ -z "$TEST_LOG" ];
        then
          echo "Successfully checked test log."
        else
          echo "Warnings/Errors/Unknowns detected in test log."
          echo "$TEST_LOG"
          exit 2
        fi
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
        packages.x86_64-linux.mensam-test
      ];
    };

}
