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
        elm make ./src/Main.elm --output $out/main.js --optimize

        echo "Uglify 'main.js'."
        uglifyjs $out/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $out/frontend.js
        rm $out/main.js
      '';
    };

  packages.x86_64-linux.elm-review =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    pkgs.buildNpmPackage {
     name = "elm-review";
     src = pkgs.fetchFromGitHub {
       owner  = "jfmengels";
       repo   = "node-elm-review";
       rev    = "9ab7d07703ba0b51370bdb8b862a8949e81ca82f";
       sha256 = "sha256-YB8i2AEAecu2/488IgQSVYTQVm7hTY+rccH9SETFU6Y=";
     };
     npmDepsHash = "sha256-BnvEdkKiFbUtEFGom9ZaCqZzId4ViGU3PlZ/BJCmX4A=";
     nativeBuildInputs = [ pkgs.coreutils ];
     buildInputs = [ elmPackages.elm elmPackages. elm-format ];
     buildPhase = ''
       substituteInPlace ./package.json \
         --replace-fail '"elm-tooling install"' '"echo skipping elm-tooling"'
       mkdir -p "$out"
       cp -r * "$out"/
       mv $out/bin/elm-review $out/bin/elm-review.js
       cat << EOF > $out/bin/elm-review
       #!${pkgs.bash}/bin/bash
       ${pkgs.nodejs}/bin/node ./elm-review.js \
         --namespace="elm-review-nix-from-src" \
         --compiler="${elmPackages.elm}/bin/elm \
         --elm-format-path="${elmPackages.elm-format}/bin/elm-format \
         "$@"
       EOF
       chmod +x $out/bin/elm-review
     '';
    };

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    mkShell {
      packages = [
        pkgs.elmPackages.elm
        pkgs.elmPackages.elm-format
        pkgs.elmPackages.elm-language-server
        packages.x86_64-linux.elm-review
        pkgs.haskellPackages.elm2nix
        pkgs.nodePackages.uglify-js
      ];
    };

  checks.x86_64-linux.package = packages.x86_64-linux.default;

  checks.x86_64-linux.devShell = devShells.x86_64-linux.default;

  checks.x86_64-linux.elm-format =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "elm-format"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        elm-format --yes src
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

  checks.x86_64-linux.elm-review =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "elm-review"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import ./elm-srcs.nix // import ./review/elm-srcs.nix;
        elmVersion = "0.19.1";
        registryDat = ./registry.dat;
      };
      installPhase = ''
        elm-review prepare-offline
        elm-review --offline
        mkdir $out
      '';
      nativeBuildInputs = [
        elmPackages.elm
        packages.x86_64-linux.elm-review
      ];
    };

}
