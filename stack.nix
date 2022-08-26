let
  pkgs = import ./nix/pkgs.nix {};
  ghc = import ./nix/ghc.nix {};
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
