{ self, nixpkgs, weeder-nix }: rec {

  packages.x86_64-linux.default = packages.x86_64-linux.staticExecutables;

  packages.x86_64-linux.staticExecutables =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    haskell.lib.justStaticExecutables packages.x86_64-linux.package;

  packages.x86_64-linux.package =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    let source = nix-gitignore.gitignoreSource [] ./.;
    in haskellPackages.callCabal2nixWithOptions "mensam" source "-fcabal2nix" {};

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    haskellPackages.shellFor {
      buildInputs = with haskellPackages; [
        cabal-install
        fourmolu
        ghcid
        haskell-language-server
        hlint
        implicit-hie
        pkgs.sqlite
        pkgs.sqlitebrowser
        weeder
      ];
      packages = haskellPackages: [
        packages.x86_64-linux.default
      ];
      withHoogle = true;
    };

  checks.x86_64-linux.staticExecutables = packages.x86_64-linux.staticExecutables;

  checks.x86_64-linux.package = packages.x86_64-linux.package;

  checks.x86_64-linux.devShell = devShells.x86_64-linux.default;

  checks.x86_64-linux.fourmolu =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    stdenv.mkDerivation {
      name = "fourmolu"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        fourmolu --mode check ./source
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        haskellPackages.fourmolu
      ];
    };

  checks.x86_64-linux.hlint =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    stdenv.mkDerivation {
      name = "hlint"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        hlint ./source
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        haskellPackages.hlint
      ];
    };

  checks.x86_64-linux.hie-yaml =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    stdenv.mkDerivation {
      name = "hie-yaml"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        diff --report-identical-files ./hie.yaml <(gen-hie)
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        haskellPackages.implicit-hie
      ];
    };

  checks.x86_64-linux.graphmod =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    stdenv.mkDerivation {
      name = "graphmod"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        graphmod > graphmod.out
        dot -Tdot graphmod.out > graphmod.dot
        dot -Tpdf graphmod.out > graphmod.pdf
      '';
      installPhase = ''
        mkdir $out
        cp graphmod.dot $out
        cp graphmod.pdf $out
      '';
      nativeBuildInputs = [
        haskellPackages.graphmod
        pkgs.graphviz
      ];
    };

  checks.x86_64-linux.weeder =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    weeder-nix.lib.x86_64-linux.makeWeederCheck {
      haskellPackages = haskellPackages.extend (self: super: { mensam = packages.x86_64-linux.package; });
      packages = [ "mensam" ];
      weederToml = ./weeder.toml;
    };

  overlays.default = final: prev: {
    haskellPackages = prev.haskellPackages.extend (haskellFinal: haskellPrev:
    let
      source = {
        selda = prev.fetchFromGitHub {
          owner  = "jumper149";
          repo   = "selda-teto";
          rev    = "6fd2b969032b144e69633b136a84be1f7f7f5111";
          sha256 = "sha256-IJCv8I1iTOCcnKVvLuavn2I/RedVWGJ4plBx7inJk4M=";
        };
        servant = prev.fetchFromGitHub {
          owner  = "haskell-servant";
          repo   = "servant";
          rev    = "v0.20.2";
          sha256 = "sha256-OW5R6RYRZ3i+xSKUILPDPNjt+yB8YJKlrIQ5TW3N3ho=";
        };
        wai-control = prev.fetchFromGitHub {
          owner  = "jumper149";
          repo   = "wai-control";
          rev    = "829c252c284e1153fc60b8a4358185d9d932959b";
          sha256 = "sha256-2lFwVyeF45x1PO1VKjbJJdR3Q6QTArcqxgURjT8Khmc=";
        };
      };
    in {
      selda = haskellPrev.callCabal2nix "selda" (source.selda.outPath + "/selda") {};
      selda-sqlite = haskellPrev.callCabal2nix "selda-sqlite" (source.selda.outPath + "/selda-sqlite") {};
      servant = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant" (source.servant.outPath + "/servant") {});
      servant-auth = haskellPrev.callCabal2nix "servant-auth" (source.servant.outPath + "/servant-auth/servant-auth") {};
      servant-auth-client = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-auth-client" (source.servant.outPath + "/servant-auth/servant-auth-client") {});
      servant-auth-server = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-auth-server" (source.servant.outPath + "/servant-auth/servant-auth-server") {});
      servant-client = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-client" (source.servant.outPath + "/servant-client") {});
      servant-client-core = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-client-core" (source.servant.outPath + "/servant-client-core") {});
      servant-server = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-server" (source.servant.outPath + "/servant-server") {});
      tzdata = haskellPrev.callHackageDirect {
        pkg = "tzdata";
        ver = "0.2.20240201.0";
        sha256 = "sha256-cN4SiPNS6Be5TYNfqW93pa5cGG+AxVCkI5dx+S8agrU=";
      } {};
      wai-control = haskellPrev.callCabal2nix "wai-control" source.wai-control.outPath {};
    });
  };

}
