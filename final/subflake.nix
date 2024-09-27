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
      directory-haddock = "${self.subflakes.server.packages.x86_64-linux.package.doc}/share/doc/mensam-0/html";
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

  dockerImages.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    pkgs.dockerTools.buildImage {
      name = "mensam";
      tag = if self ? rev then self.rev else null;
      copyToRoot =
        pkgs.buildEnv {
          name = "mensam-root-docker";
          pathsToLink = [
            "/bin"
            "/etc/mensam"
            "/usr/share/mensam"
            "/var/lib/mensam"
            "/var/log/mensam"
          ];
          paths = [
            pkgs.bash
            pkgs.coreutils
            (stdenv.mkDerivation {
              name = "mensam-static-docker"; # TODO: Necessary to avoid segmentation fault.
              dontUnpack = true;
              buildPhase = ''
                # Configuration
                mkdir --parents build/etc/mensam

                # Source documentation
                mkdir --parents build/usr/share/mensam/haddock
                cp --recursive ${self.subflakes.server.packages.x86_64-linux.package.doc}/share/doc/mensam-0/html/. build/usr/share/mensam/haddock

                # Static files
                mkdir --parents build/usr/share/mensam/static
                cp --recursive ${self.subflakes.static.packages.x86_64-linux.default.outPath}/. build/usr/share/mensam/static

                # Database
                mkdir --parents build/var/lib/mensam
              '';
              installPhase = ''
                cp --recursive build $out
              '';
            })
          ];
      };
      config = {
        Cmd = [ "${self.subflakes.server.packages.x86_64-linux.default}/bin/mensam-server" ];
        Env = [
          "MENSAM_CONFIG_FILE=/etc/mensam/mensam.json"
          "MENSAM_LOG_COLOR=True"
          #"MENSAM_LOG_FILE=" # Docker expects the log on StdOut.
          "MENSAM_LOG_LEVEL=LevelDebug"
        ];
        Volumes = {
          "/var/lib/mensam" = { };
        };
      };
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

  checks.x86_64-linux.dockerImage = dockerImages.default;

}
