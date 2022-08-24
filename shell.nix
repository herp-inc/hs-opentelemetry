let
  pkgs = import ./nix/pkgs.nix {};
  settings = import ./nix/ghc.nix {};
in
  with pkgs;
  mkShell {
    buildInputs = [
      niv

      settings.ghc
      stack
      cabal-install
      hpack
      haskell-language-server
      stylish-haskell
      hlint

      zlib
    ];
  }
