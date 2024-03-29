{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default overlays.default ]; };
    let
      source = nix-gitignore.gitignoreSource [] ./.;
      package = (haskellPackages.callCabal2nixWithOptions "mensam" source "-fcabal2nix" {});
      executables = haskell.lib.justStaticExecutables package;
    in executables;

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

  overlays.default = final: prev: {
    haskellPackages = prev.haskell.packages.ghc944.extend (haskellFinal: haskellPrev:
    let
      source = {
        password = prev.fetchFromGitHub {
          owner  = "cdepillabout";
          repo   = "password";
          rev    = "c0102383537f33a3da928896a57e4e3dca4dd163";
          sha256 = "sha256-VSmTGZuQX8iFpDo+0mfDo1GfgTT85EMyfR791drPIf4=";
        };
        selda = prev.fetchFromGitHub {
          owner  = "valderman";
          repo   = "selda";
          rev    = "ab9619db13b93867d1a244441bb4de03d3e1dadb";
          sha256 = "sha256-P0nqAYzbeTyEEgzMij/3mKcs++/p/Wgc7Y6bDudXt2U=";
        };
        servant = prev.fetchFromGitHub {
          owner  = "haskell-servant";
          repo   = "servant";
          rev    = "a082794a48546ffd681f4206436c59b9c1f901e1";
          sha256 = "sha256-5rqcUECYi587f/dyCA/SSQxL5KedE4kdP6jW1L0eLaI=";
        };
      };
    in {
      attoparsec-iso8601 = haskellPrev.callHackage "attoparsec-iso8601" "1.1.0.0" {};
      brick = haskellPrev.callHackage "brick" "1.6" {};
      deriving-trans = haskellPrev.callHackage "deriving-trans" "0.5.1.0" {};
      ghcid = prev.haskell.lib.dontCheck haskellPrev.ghcid;
      http-api-data = haskellPrev.callHackage "http-api-data" "0.5" {};
      jose = haskellPrev.callHackage "jose" "0.10" {};
      microlens = haskellPrev.callHackage "microlens" "0.4.13.1" {};
      microlens-ghc = haskellPrev.callHackage "microlens-ghc" "0.4.14.1" {};
      microlens-platform = haskellPrev.callHackage "microlens-platform" "0.4.3.1" {};
      openapi3 = (prev.haskell.lib.dontCheck haskellPrev.openapi3).overrideAttrs (oldAttrs: { meta = oldAttrs.meta // { broken = false; }; });
      password = haskellPrev.callCabal2nix "password" (source.password.outPath + "/password") {};
      password-types = haskellPrev.callCabal2nix "password-types" (source.password.outPath + "/password-types") {};
      resource-pool = haskellPrev.callHackage "resource-pool" "0.4.0.0" {};
      selda = haskellPrev.callCabal2nix "selda" (source.selda.outPath + "/selda") {};
      selda-sqlite = haskellPrev.callCabal2nix "selda-sqlite" (source.selda.outPath + "/selda-sqlite") {};
      servant = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant" (source.servant.outPath + "/servant") {});
      servant-auth = haskellPrev.callCabal2nix "servant-auth" (source.servant.outPath + "/servant-auth/servant-auth") {};
      servant-auth-client = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-auth-client" (source.servant.outPath + "/servant-auth/servant-auth-client") {});
      servant-auth-server = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-auth-server" (source.servant.outPath + "/servant-auth/servant-auth-server") {});
      servant-client = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-client" (source.servant.outPath + "/servant-client") {});
      servant-client-core = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-client-core" (source.servant.outPath + "/servant-client-core") {});
      servant-server = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-server" (source.servant.outPath + "/servant-server") {});
      singletons = haskellPrev.callHackage "singletons" "3.0.2" {};
      vty = haskellPrev.callHackage "vty" "5.38" {};
    });
  };

}
