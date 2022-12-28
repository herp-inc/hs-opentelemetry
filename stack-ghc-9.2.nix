let
  pkgs = import ./nix/pkgs.nix {};
  ghc = import ./nix/ghc.nix { ghcVersion = "923"; };
in
  with pkgs;
  haskell.lib.buildStackProject {
    buildInputs = [
      mysql57
      openssl
      pcre
      postgresql
      zlib
    ];
    ghc = ghc.ghc;
    name = "hs-opentelemetry";
  }
