{ self }: rec {

  overlays.default = final: prev: {
    haskellPackages = prev.haskell.packages.ghc925.extend (haskellFinal: haskellPrev: { # TODO: Using GHC 9.2.5.
      graphmod = (haskellPrev.graphmod.overrideAttrs (oldAttrs: {
        src = prev.fetchFromGitHub {
          owner = "yav";
          repo = "graphmod";
          rev = "ad3f136c6bcebd2b7e6fa1d5a1006436008acd3d";
          sha256 = "sha256-2JESSCR6AedyPFo0GlQaSa9vKzzoHIJ6uxZ8IQVjYdA=";
        };
      }));
    });
  };

  deployment = builtins.fromJSON (builtins.readFile ./deployment.json);

  config = builtins.fromJSON (builtins.readFile ./configurations/${deployment}.json);

}
