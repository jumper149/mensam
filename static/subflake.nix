{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "static"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        make all

        cp -r ${packages.x86_64-linux.fonts}/fonts build

        cp ${pkgs.redocly-cli}/lib/node_modules/@redocly/cli/node_modules/redoc/bundles/redoc.standalone.js build

        sed -i 's|https://cdn.redoc.ly/redoc/logo-mini.svg|static/favicon.png|g' build/redoc.standalone.js

        cp --target-directory=build --recursive ${self.subflakes.frontend.packages.x86_64-linux.default.outPath}/*
      '';
      installPhase = ''
        cp --recursive build $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        imagemagick
      ];
    };

  # Mensam: Fira
  # Redoc: Montserrat, Roboto
  packages.x86_64-linux.fonts =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "fonts"; # TODO: Necessary to avoid segmentation fault.
      dontUnpack = true;
      buildPhase = ''
        mkdir -p build
        cp -r ${pkgs.fira.outPath}/share/fonts/opentype build
        mv build/opentype build/fonts
        chmod --recursive +w build/fonts
        cp ${pkgs.roboto.outPath}/share/fonts/truetype/* build/fonts
        for f in build/fonts/*
        do
          woff2_compress $f
        done
        cp ${pkgs.montserrat.outPath}/share/fonts/woff2/* build/fonts
      '';
      installPhase = ''
        cp --recursive build $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        woff2
      ];
    };

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    pkgs.mkShell {
      inputsFrom = [
        packages.x86_64-linux.default
        packages.x86_64-linux.fonts
      ];
    };

  checks.x86_64-linux.package = packages.x86_64-linux.default;

  checks.x86_64-linux.fonts = packages.x86_64-linux.fonts;

  checks.x86_64-linux.devShell = devShells.x86_64-linux.default;

}
