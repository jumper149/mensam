{ self, nixpkgs }: rec {

  overlays.default = final: prev: {
  };

  dockerImages.devcontainer =
    let
      source =
        with import nixpkgs { system = "x86_64-linux"; overlays = [ overlays.default ]; };
        {
          docker-nixpkgs = pkgs.fetchFromGitHub {
            owner  = "nix-community";
            repo   = "docker-nixpkgs";
            rev    = "64a3352bbfcca09a04774880951b0030e8f01409";
            sha256 = "sha256-637nBKXQmFcYV/gg+ivWOBcPTdwPTlI8aBsUZ9vYYLI=";
          };
        };
      docker-nixpkgs-pkgs = import nixpkgs {
        config = { };
        system = "x86_64-linux";
        overlays = [
          overlays.default
          (import "${source.docker-nixpkgs}/overlay.nix")
        ];
      };
    in
    with import nixpkgs { system = "x86_64-linux"; overlays = [ overlays.default ]; };
    pkgs.dockerTools.buildImage {
      name = "mensam-devcontainer";
      fromImage = docker-nixpkgs-pkgs.docker-nixpkgs.devcontainer;
      contents = [
        pkgs.bashInteractive
        pkgs.bash-completion
        pkgs.cacert
        pkgs.direnv
        pkgs.nix-direnv
        (writeTextFile {
          name = "direnvrc";
          destination = "/root/.config/direnv/direnvrc";
          text = ''
            source /share/nix-direnv/direnvrc
          '';
        })
        (writeTextFile {
          name = "direnv.toml";
          destination = "/root/.config/direnv/direnv.toml";
          text = ''
            [whitelist]
              prefix = [ "/" ]
          '';
        })
        pkgs.git
        (writeTextFile {
          name = "bashrc";
          destination = "/etc/bashrc";
          text = builtins.readFile ./bashrc;
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
        (pkgs.writeTextFile {
          name = "docker-entrypoint.sh";
          destination = "/etc/docker-entrypoint.sh";
          executable = true;
          text = ''
            #!/bin/bash

            # GitHub Codespaces fix: https://github.com/xtruder/nix-devcontainer/blob/42d11ec3fdac6cda5342b4dc83adbcb54d587038/src/docker-entrypoint.sh#L3
            if [ "$\{CODESPACES\}" = true ]; then
              # vscode codespaces set default permissions on /tmp. These will
              # produce invalid permissions on files built with nix. This fix
              # removes default permissions set on /tmp
              sudo setfacl --remove-default /tmp
            fi

            sleep infinity
          '';
        })
      ];
      config = {
        Entrypoint = [ "/etc/docker-entrypoint.sh" ];
      };
    };

}
