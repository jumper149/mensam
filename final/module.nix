{ finalOverlay }:
  { config, lib, pkgs, ... }: {
    options = {
      services.mensam = {
        enable = lib.mkEnableOption "Mensam.";
        provider = lib.mkOption {
          default = "nix";
          type = lib.types.enum [ "nix" "docker" ];
          description = ''
            Choose between "nix" and "docker".
            "nix" uses an executable from the nix store and sets up a systemd service.
            "docker" uses a docker image to provide the executable and runs an oci-container with podman as a systemd service.
          '';
        };
        environment = lib.mkOption {
          default = { };
          type = with lib.types; attrsOf string;
          description = ''
            Environment variables to overwrite the default environment variables.
          '';
        };
        config = lib.mkOption {
          default = { };
          type = with lib.types; attrsOf anything;
          description = ''
            Configuration that will be merged with default options and serialized to JSON.
            `lib.recursiveUpdate` is used to merge these changes.
          '';
        };
      };
    };
    config = lib.mkIf config.services.mensam.enable (lib.mkMerge [
      {
        environment = {
          etc."mensam.json".text = builtins.toJSON (
            lib.recursiveUpdate {
                nix = pkgs.mensam.config.default;
                docker = pkgs.mensam.config.docker;
              }.${config.services.mensam.provider}
              config.services.mensam.config
          );
        };
        nixpkgs.overlays = [
          finalOverlay
        ];
        users.users = {
          mensam = {
            uid = 377;
            group = "mensam";
            description = "Mensam user";
            home = "/var/lib/mensam";
          };
        };
        users.groups = {
          mensam.gid = 377;
        };
      }

      {
        nix = {
          systemd.services.mensam = {
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            description = "Mensam";
            environment = {
              MENSAM_CONFIG_FILE = "/etc/mensam.json";
              MENSAM_LOG_COLOR = "False";
              MENSAM_LOG_FILE = "/var/log/mensam/access.log";
              MENSAM_LOG_LEVEL = "LevelInfo";
            } // config.services.mensam.environment;
            restartTriggers = [
              config.environment.etc."mensam.json".source
            ];
            serviceConfig = {
              ExecStart = "${pkgs.mensam.exe}/bin/mensam-server";
              LogsDirectory = "mensam";
              StateDirectory = "mensam";
              User = "mensam";
            };
          };
        };

        docker = {
          virtualisation.podman.enable = true;
          virtualisation.oci-containers.backend = "podman";
          services.systemd."mensam".serviceConfig = {
              LogsDirectory = "mensam";
              StateDirectory = "mensam";
              User = "mensam";
            };
          virtualisation.oci-containers.containers."mensam" = {
            serviceName = "mensam";
            image = "docker.io/jumper149/mensam:${pkgs.mensam.revision}";
            # ports = [
            #   "127.0.0.1:8177:8177"
            # ];
            extraOptions = [
              "--network=host" # Share all ports with the regular operating system.
            ];
            podman.user = "mensam";
            volumes = [
              "/var/lib/mensam:/var/lib/mensam"
              "/var/log/mensam:/var/log/mensam"
              "/etc/mensam.json:/etc/mensam.json:ro"
              "${pkgs.cacert.outPath}/etc/ssl/certs/ca-bundle.crt:/etc/ssl/certs/ca-bundle.crt:ro"
            ];
            environment = {
              MENSAM_CONFIG_FILE = "/etc/mensam.json";
              MENSAM_LOG_COLOR = "False";
              MENSAM_LOG_FILE = "/var/log/mensam/access.log";
              MENSAM_LOG_LEVEL = "LevelInfo";
            } // config.services.mensam.environment;
          };
        };
      }.${config.services.mensam.provider}
    ]);
  }
