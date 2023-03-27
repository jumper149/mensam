{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "mensam-server-full" ''
      MENSAM_CONFIG_FILE="${self.subflakes.config.packages.x86_64-linux.default}" ${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-server
    '';

  packages.x86_64-linux.mensam-init =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "mensam-init-full" ''
      export MENSAM_CONFIG_FILE="${self.subflakes.config.packages.x86_64-linux.default}"
      export MENSAM_LOG_LEVEL=LevelWarn
      ${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-init
    '';

  packages.x86_64-linux.mensam-test =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "mensam-test-full" ''
      export MENSAM_CONFIG_FILE="${self.subflakes.config.packages.x86_64-linux.default}"
      export MENSAM_LOG_LEVEL=LevelWarn
      ${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-test
    '';

  packages.x86_64-linux.mensam-client =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeScriptBin "mensam-client-full" ''
      ${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-client
    '';

  overlays.default = final: prev: {
    mensam = {
      exe = self.subflakes.server.packages.x86_64-linux.default;
      full = packages.x86_64-linux.default;
      config.default = self.subflakes.config.config;
    };
  };

  nixosModules.default = import ./module.nix {
    finalOverlay = overlays.default;
  };

  checks.x86_64-linux.mensam-test =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "mensam-test"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        set +e
        INIT_LOG="$(mensam-init-full || echo "Server initialization exited abnormally.")"
        set -e
        echo "$INIT_LOG"

        if [ -z "$INIT_LOG" ];
        then
          echo "Successfully checked initialization log."
        else
          echo "Warnings/Errors/Unknowns detected in initialization log."
          echo "$INIT_LOG"
          exit 1
        fi

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
        packages.x86_64-linux.mensam-init
        packages.x86_64-linux.mensam-test
      ];
    };

}
