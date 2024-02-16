{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=21.11";
    nixpkgs2311.url = "github:NixOS/nixpkgs?ref=23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nixpkgs2311, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgs2311 = import nixpkgs2311 { inherit system; };
        compiler = "ghc901";
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskell.compiler.${compiler}
            cabal-install
            pkgs2311.stack
            pkgs2311.hpack

            pkgs2311.haskell.packages.ghc96.implicit-hie
            haskell.packages.${compiler}.haskell-language-server
            pkgs2311.haskell.packages.ghc96.hspec-discover
            pkgs2311.haskell.packages.ghc96.fourmolu

            awscli
            glibc
            grpc
            libffi
            mysql80
            nixpkgs-fmt
            openssl
            pcre
            postgresql
            stdenv
            zlib
            zstd
          ];
        };
      }
    );
}
