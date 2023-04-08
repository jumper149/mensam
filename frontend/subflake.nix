{ self, nixpkgs }: rec {

  devShells.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    mkShell {
      packages = [
        pkgs.elmPackages.elm
        pkgs.elmPackages.elm-language-server
      ];
    };

}
