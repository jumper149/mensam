{ finalOverlay }:
  { config, lib, pkgs, ... }: {
    options = {
      services.homepage = {
        enable = lib.mkEnableOption "Felix Springer's Homepage.";
        config = lib.mkOption {
          default = { };
          type = with lib.types; attrsOf anything;
          description = ''
            Configuration, that will be merged with default options and serialized to JSON.
            `lib.recursiveUpdate` is used to merge these changes.
          '';
        };
      };
    };
    config = lib.mkIf config.services.homepage.enable {
      environment = {
        etc."homepage.json".text = builtins.toJSON (
          lib.recursiveUpdate pkgs.homepage-jumper149.config.default config.services.homepage.config
        );
      };
      nixpkgs.overlays = [
        finalOverlay
      ];
      systemd.services.homepage = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        description = "Homepage";
        environment = {
          HOMEPAGE_CONFIG_FILE = "/etc/homepage.json";
          HOMEPAGE_LOG_FILE = "/var/log/homepage/access.log";
          HOMEPAGE_LOG_LEVEL = "LevelInfo";
        };
        restartTriggers = [
          config.environment.etc."homepage.json".source
        ];
        serviceConfig = {
          DynamicUser = true;
          ExecStart = "${pkgs.homepage-jumper149.exe}/bin/homepage";
          LogsDirectory = "homepage";
        };
      };
    };
  }
