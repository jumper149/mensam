{ self, nixpkgs }: rec {

  overlays.default = final: prev: {
    nix = prev.nixVersions.nix_2_19;
  };

  dockerImages.devcontainer =
    let
      source =
        with import nixpkgs { system = "x86_64-linux"; overlays = [ overlays.default ]; };
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

}
