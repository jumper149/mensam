{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "mensam-elm";
      src = ./.;

      buildInputs = [
        elmPackages.elm
        nodePackages.uglify-js
      ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import ./elm-srcs.nix;
        elmVersion = "0.19.1";
        registryDat = ./registry.dat;
      };

      installPhase = ''
        mkdir -p $out

        echo "Compile 'Main' module."
        elm make ./source/Main.elm --output $out/main.js --optimize

        echo "Uglify 'main.js'."
        uglifyjs $out/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $out/frontend.js
        rm $out/main.js
      '';
    };

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    mkShell {
      packages = [
        pkgs.elmPackages.elm
        pkgs.elmPackages.elm-format
        pkgs.elmPackages.elm-language-server
        pkgs.elmPackages.elm-review
        pkgs.haskell.packages.ghc926.elm2nix
        pkgs.nodePackages.uglify-js
      ];
    };

  checks.x86_64-linux.elm-format =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "elm-format"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        elm-format --yes source
        elm-format --yes review/src
      '';
      installPhase = ''
        mkdir $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        pkgs.elmPackages.elm-format
      ];
    };

}
