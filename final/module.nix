{ finalOverlay }:
  { config, lib, pkgs, ... }: {
    options = {
      services.mensam = {
        enable = lib.mkEnableOption "Mensam.";
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
    config = lib.mkIf config.services.mensam.enable {
      environment = {
        etc."mensam.json".text = builtins.toJSON (
          lib.recursiveUpdate pkgs.mensam.config.default config.services.mensam.config
        );
      };
      nixpkgs.overlays = [
        finalOverlay
      ];
      systemd.services.mensam = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        description = "Mensam";
        environment = {
          MENSAM_CONFIG_FILE = "/etc/mensam.json";
          MENSAM_LOG_FILE = "/var/log/mensam/access.log";
          MENSAM_LOG_LEVEL = "LevelInfo";
        } // config.services.mensam.environment;
        restartTriggers = [
          config.environment.etc."mensam.json".source
        ];
        serviceConfig = {
          DynamicUser = true;
          ExecStart = "${pkgs.mensam.exe}/bin/mensam-server";
          LogsDirectory = "mensam";
          StateDirectory = "mensam";
        };
      };
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
    };
  }
