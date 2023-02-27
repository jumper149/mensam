{ self, nixpkgs }: rec {

  packages.x86_64-linux.default =
    with import nixpkgs { system = "x86_64-linux"; overlays = [ self.subflakes.setup.overlays.default ]; };
    writeText "homepage.json" (builtins.toJSON config);

  config =
    self.subflakes.setup.config // {
      revision = if self ? rev then self.rev else null;
      directory-static = "${self.subflakes.static.packages.x86_64-linux.default}";
    };

}
