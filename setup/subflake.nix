{ self }: rec {

  overlays.default = final: prev: {
    haskellPackages = prev.haskell.packages.ghc944.extend (haskellFinal: haskellPrev:
    let
      source = {
        password = prev.fetchFromGitHub {
          owner  = "cdepillabout";
          repo   = "password";
          rev    = "c0102383537f33a3da928896a57e4e3dca4dd163";
          sha256 = "sha256-VSmTGZuQX8iFpDo+0mfDo1GfgTT85EMyfR791drPIf4=";
        };
        selda = prev.fetchFromGitHub {
          owner  = "valderman";
          repo   = "selda";
          rev    = "ab9619db13b93867d1a244441bb4de03d3e1dadb";
          sha256 = "sha256-P0nqAYzbeTyEEgzMij/3mKcs++/p/Wgc7Y6bDudXt2U=";
        };
        servant = prev.fetchFromGitHub {
          owner  = "haskell-servant";
          repo   = "servant";
          rev    = "a082794a48546ffd681f4206436c59b9c1f901e1";
          sha256 = "sha256-5rqcUECYi587f/dyCA/SSQxL5KedE4kdP6jW1L0eLaI=";
        };
      };
    in {
      attoparsec-iso8601 = haskellPrev.callHackage "attoparsec-iso8601" "1.1.0.0" {};
      deriving-trans = haskellPrev.callHackage "deriving-trans" "0.5.1.0" {};
      ghcid = prev.haskell.lib.dontCheck haskellPrev.ghcid;
      http-api-data = haskellPrev.callHackage "http-api-data" "0.5" {};
      jose = haskellPrev.callHackage "jose" "0.10" {};
      password = haskellPrev.callCabal2nix "password" (source.password.outPath + "/password") {};
      password-types = haskellPrev.callCabal2nix "password-types" (source.password.outPath + "/password-types") {};
      resource-pool = haskellPrev.callHackage "resource-pool" "0.4.0.0" {};
      selda = haskellPrev.callCabal2nix "selda" (source.selda.outPath + "/selda") {};
      selda-sqlite = haskellPrev.callCabal2nix "selda-sqlite" (source.selda.outPath + "/selda-sqlite") {};
      servant = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant" (source.servant.outPath + "/servant") {});
      servant-auth = haskellPrev.callCabal2nix "servant-auth" (source.servant.outPath + "/servant-auth/servant-auth") {};
      servant-auth-server = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-auth-server" (source.servant.outPath + "/servant-auth/servant-auth-server") {});
      servant-server = prev.haskell.lib.dontCheck (haskellPrev.callCabal2nix "servant-server" (source.servant.outPath + "/servant-server") {});
      singletons = haskellPrev.callHackage "singletons" "3.0.2" {};
    });
  };

  deployment = builtins.fromJSON (builtins.readFile ./deployment.json);

  config = builtins.fromJSON (builtins.readFile ./configurations/${deployment}.json);

}
