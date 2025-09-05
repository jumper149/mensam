moduleMensam: { config, pkgs, ... }:
let
  my-domain = "my-domain.com";
in {
  imports = [ moduleMensam ];

  networking.firewall.allowedTCPPorts = [
    80 # HTTP
    443 # HTTPS
  ];

  security.acme = {
    acceptTerms = true;
    certs = {
      "${my-domain}" = {
        reloadServices = [
          "dovecot.service"
          "postfix.service"
        ];
      };
      "mensam.${my-domain}" = {
        reloadServices = [
          "nginx.service"
        ];
      };
    };
    defaults = {
      email = "test@example.com";
      server = "https://acme-staging-v02.api.letsencrypt.org/directory";
      webroot = "/var/lib/acme/acme-challenge";
    };
    useRoot = true;
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    virtualHosts."mensam.${my-domain}" = {
      enableACME = true;
      extraConfig = ''
        error_page 501 502 503 /fallback.html;
        location /fallback.html {
          root ${pkgs.mensam.full.fallback.outPath};
          add_header Cache-Control "no-store";
        }
      '';
      forceSSL = true;
      locations."/".proxyPass = "http://127.0.0.1:${toString config.services.mensam.config.port}";
      locations."/".extraConfig = ''
        add_header Cache-Control "no-store";
      '';
    };
  };

  systemd.services.nginx.serviceConfig.SupplementaryGroups = [
    config.security.acme.certs."${my-domain}".group
    config.security.acme.certs."mensam.${my-domain}".group
  ];

  services.mensam = {
    enable = true;
    config = {
      base-url = {
        scheme = "https";
        authority = {
          host = "mensam.${my-domain}";
          port = null;
        };
        path = [];
      };
      email-config = {
        hostname = my-domain;
        username = "mensam@${my-domain}";
        password = "********";
        port = 465;
        tls = true;
      };
      port = 8177;
    };
  };
}
