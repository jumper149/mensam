{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    stdenv.mkDerivation {
      name = "fallback"; # TODO: Necessary to avoid segmentation fault.
      src = ./.;
      buildPhase = ''
        cp --recursive source build
      '';
      installPhase = ''
        cp --recursive build $out
      '';
      buildInputs = [
      ];
    };

  checks.x86_64-linux.package = packages.x86_64-linux.default;

}
