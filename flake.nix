{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=26.05";
    nixpkgs2311.url = "github:NixOS/nixpkgs?ref=23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nixpkgs2311, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        compiler = "ghc96";
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler.${compiler}
            cabal-install
            stack
            hpack

            haskell.packages.${compiler}.implicit-hie
            haskell.packages.${compiler}.haskell-language-server
            haskell.packages.${compiler}.hspec-discover
            haskell.packages.${compiler}.fourmolu

            awscli
            grpc
            libffi
            mysql84
            nixpkgs-fmt
            openssl
            pcre
            postgresql.pg_config
            stdenv
            zlib
            zstd
          ];
        };
      }
    );
}
