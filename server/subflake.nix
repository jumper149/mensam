{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    let
      source = nix-gitignore.gitignoreSource [] ./.;
      package = (haskellPackages.callCabal2nixWithOptions "homepage" source "-fcabal2nix" {});
      executables = haskell.lib.justStaticExecutables package;
    in executables;

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    haskellPackages.shellFor {
      buildInputs = with haskellPackages; [
        cabal-install
        calligraphy
        pkgs.findutils
        fourmolu
        ghcid
        haskell-language-server
        hlint
        implicit-hie
        weeder
        pkgs.xdot
      ];
      packages = haskellPackages: [
        packages.x86_64-linux.default
      ];
      withHoogle = true;
    };

  checks.x86_64-linux.fourmolu =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
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
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
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
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
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
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
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

}
