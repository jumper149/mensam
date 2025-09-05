moduleMensam: { config, pkgs, ... }:
let
  my-domain = "my-domain.com";
  selfSignedCert = pkgs.runCommand
    "self-signed-cert"
    { nativeBuildInputs = [ pkgs.openssl ]; }
    ''
      mkdir -p $out
      openssl req -x509 -nodes -days 365 \
        -subj "/CN=mensam.${my-domain}" \
        -newkey rsa:2048 \
        -keyout $out/privkey.pem \
        -out $out/fullchain.pem
    '';
in {
  imports = [ moduleMensam ];

  networking.firewall.allowedTCPPorts = [
    80 # HTTP
    443 # HTTPS
  ];

  # # This would generate certificates for HTTPS.
  # security.acme = {
  #   acceptTerms = true;
  #   certs = {
  #     "${my-domain}" = {
  #       reloadServices = [
  #         "dovecot.service"
  #         "postfix.service"
  #       ];
  #     };
  #     "mensam.${my-domain}" = {
  #       reloadServices = [
  #         "nginx.service"
  #       ];
  #     };
  #   };
  #   defaults = {
  #     email = "test@example.com";
  #     server = "https://acme-staging-v02.api.letsencrypt.org/directory";
  #     webroot = "/var/lib/acme/acme-challenge";
  #   };
  # };

  # Using self-signed certificate instead of ACME setup.
  environment.etc."ssl/mensam.${my-domain}/fullchain.pem".source = "${selfSignedCert}/fullchain.pem";
  environment.etc."ssl/mensam.${my-domain}/privkey.pem".source = "${selfSignedCert}/privkey.pem";

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    virtualHosts."mensam.${my-domain}" = {
      # enableACME = true;
      sslCertificate = "/etc/ssl/mensam.${my-domain}/fullchain.pem";
      sslCertificateKey = "/etc/ssl/mensam.${my-domain}/privkey.pem";
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

  # # If we were using ACME, we would have to allow nginx to read the certificate.
  # systemd.services.nginx.serviceConfig.SupplementaryGroups = [
  #   config.security.acme.certs."${my-domain}".group
  #   config.security.acme.certs."mensam.${my-domain}".group
  # ];

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
      port = 8177; # The local port that is used for communication between nginx and mensam. This port is not open.
    };
  };
}
