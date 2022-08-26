let
  pkgs = import ./nix/pkgs.nix {};
  ghc = import ./nix/ghc.nix {};
in
  with pkgs;
  mkShell {
    buildInputs = [
      niv

      ghc.ghc
      stack
      cabal-install
      hpack
      haskell-language-server
      stylish-haskell
      hlint

      mysql57
      openssl
      pcre
      postgresql
      zlib
    ];
  }
