{
  description = "Haskell OpenTelemetry support.";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=26.05";
    nixpkgs2311.url = "github:NixOS/nixpkgs?ref=23.11";
    flake-utils.url = "github:numtide/flake-utils";
    devenv.url = "github:cachix/devenv/v1.0.5";
    # Hack to avoid needing to use impure when loading the devenv root.
    #
    # See .envrc for how we substitute this with the actual path.
    #
    # Alternatively, use the --impure flag when running nix develop, nix show, etc.
    # devenv-root = {
    #   url = "file+file:///dev/null";
    #   flake = false;
    # };

    otlp-protobufs = {
      url = "github:open-telemetry/opentelemetry-proto/b43e9b18b76abf3ee040164b55b9c355217151f3";
      flake = false;
    };
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
    in {
      packages =
        {
          # devenv-up = self.devShells.${system}.default.config.procfileScript;
        }
        // haskellPackageUtils.localPackageMatrix;

      devShells = rec {
        default = ghc96;
        # ghc810 = mkShellForGHC "ghc810";
        # ghc90 = mkShellForGHC "ghc90";
        ghc92 = mkShellForGHC "ghc92";
        ghc94 = mkShellForGHC "ghc94";
        ghc96 = mkShellForGHC "ghc96";
        ghc98 = mkShellForGHC "ghc98";
        ghc910 = mkShellForGHC "ghc98";
      };

      checks = {
        pre-commit-check = devenv.inputs.pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = pre-commit-hooks;
        };
      };
    });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.garnix.io"
      "https://devenv.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    ];
    allow-import-from-derivation = "true";
  };
}
