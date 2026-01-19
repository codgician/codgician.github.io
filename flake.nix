{
  description = "☕️ codgician's personal homepage.";

  nixConfig = {
    allow-import-from-derivation = "true";
    extra-substituters = [ "https://cache.garnix.io" ];
    extra-trusted-public-keys = [ "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hPkgs = pkgs.haskell.packages."ghc984";

        myDevTools = [
          hPkgs.ghc
          hPkgs.ghcid
          hPkgs.ormolu
          hPkgs.hlint
          hPkgs.hoogle
          hPkgs.haskell-language-server
          hPkgs.implicit-hie
          hPkgs.retrie
          stack-wrapped
          pkgs.zlib
          pkgs.dart-sass
          pkgs.nodePackages.katex
          pkgs.mermaid-cli
        ];

        # Wrap Stack to work with our Nix integration. We don't want to modify
        # stack.yaml so non-Nix users don't notice anything.
        # - no-nix: We don't want Stack's way of integrating Nix.
        # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
        # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = myDevTools;

          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          # https://github.com/NixOS/nixpkgs/blob/d64780ea0e22b5f61cd6012a456869c702a72f20/pkgs/development/haskell-modules/generic-stack-builder.nix#L38
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
        };

        formatter = pkgs.writeShellApplication {
          name = "formatter";
          runtimeInputs = with pkgs; [
            treefmt
            nixfmt-rfc-style
            mdformat
            yamlfmt
            hPkgs.ormolu
          ];
          text = with pkgs; lib.getExe treefmt;
        };
      }
    );
}
