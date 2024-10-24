{ self }: rec {

  overlays.default = final: prev: {
    nix = prev.nixVersions.nix_2_19;
  };

}
