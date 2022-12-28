{ pkgs ? import ./pkgs.nix {}, ghcVersion }:
rec {
  ghc = pkgs.haskell.compiler.${compiler};
  compiler = "ghc${ghcVersion}";
}
