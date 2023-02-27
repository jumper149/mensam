{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "static"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        make all
      '';
      installPhase = ''
        cp --recursive build $out
      '';
      buildInputs = [
      ];
      nativeBuildInputs = [
        imagemagick
        lessc
      ];
    };

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    pkgs.mkShell {
      inputsFrom = [
        packages.x86_64-linux.default
      ];
    };

}
