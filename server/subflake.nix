{ self, nixpkgs, weeder-nix }: rec {

  packages.x86_64-linux.default =
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
          owner  = "teto";
          repo   = "selda";
          rev    = "c6790f1a3d5edfe547594206860bb49733bc0e48";
          sha256 = "sha256-jmMBFs5GVihyH7DqLAWTeT6EBDChYeNBKxuUJNWYe+w=";
        };
        servant = prev.fetchFromGitHub {
          owner  = "haskell-servant";
          repo   = "servant";
          rev    = "ef4b38a342469ba2a73b6a4aec07c915a43bd2f3";
          sha256 = "sha256-J8Yp+Q9N160Q2kuYxMzFUNwue7l0nRSVe6inO4kNyFI=";
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
    });
  };

}
