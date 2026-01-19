{
  description = "☕️ codgician's personal homepage.";

  nixConfig = {
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
        hPkgs = pkgs.haskellPackages;

        # Source files for the site builder
        builderSrc = pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./src
            ./app
            ./test
            ./package.yaml
            ./builder.cabal
            ./LICENSE
            ./CHANGELOG.md
          ];
        };

        # The Hakyll site generator binary
        siteBuilder = hPkgs.callCabal2nix "builder" builderSrc { };

        # External tools needed for building
        buildTools = [
          pkgs.dart-sass
          pkgs.nodePackages.katex
          pkgs.mermaid-cli
        ];

        # KaTeX dist path for CSS and fonts
        katexDist = "${pkgs.nodePackages.katex}/lib/node_modules/katex/dist";

        # Lucide icons (icon font with CSS)
        lucideStatic = pkgs.fetchzip {
          url = "https://registry.npmjs.org/lucide-static/-/lucide-static-0.544.0.tgz";
          hash = "sha256-8+MABl6ToG4e3SM7VEzoDoHsCY52gtlMVW9EuOk5fd0=";
          stripRoot = true;
        };
        lucideFont = "${lucideStatic}/font";

        # The final website derivation
        website = pkgs.stdenv.mkDerivation {
          name = "codgician-site";
          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./content
              ./templates
              ./static
              ./config.yaml
            ];
          };
          nativeBuildInputs = [ siteBuilder ] ++ buildTools;

          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = pkgs.lib.optionalString (
            pkgs.stdenv.hostPlatform.libc == "glibc"
          ) "${pkgs.glibcLocales}/lib/locale/locale-archive";

          KATEX_VERSION = pkgs.nodePackages.katex.version or "unknown";
          MERMAID_VERSION = pkgs.mermaid-cli.version or "unknown";

          buildPhase = ''
            runHook preBuild

            # Copy KaTeX CSS and fonts into static/ so Hakyll picks them up
            mkdir -p static/vendor/katex
            cp ${katexDist}/katex.min.css static/vendor/katex/
            cp -r ${katexDist}/fonts static/vendor/katex/

            # Copy Lucide icon font CSS and webfonts
            mkdir -p static/vendor/lucide
            cp ${lucideFont}/lucide.css static/vendor/lucide/
            cp ${lucideFont}/lucide.woff2 static/vendor/lucide/

            site build
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp -r _site/* $out/
            runHook postInstall
          '';
        };

        # GHC with all dependencies for development
        ghcWithDeps = hPkgs.ghcWithPackages (ps: [
          ps.hakyll
          ps.pandoc
          ps.pandoc-types
          ps.text
          ps.bytestring
          ps.filepath
          ps.directory
          ps.process
          ps.yaml
          ps.aeson
          ps.time
          ps.containers
          ps.cryptohash-sha256
          ps.base16-bytestring
          ps.temporary
          ps.hspec
        ]);

        # Development environment with HLS and all tools
        devTools = [
          hPkgs.cabal-install
          hPkgs.haskell-language-server
          hPkgs.ormolu
          hPkgs.hlint
          pkgs.pkg-config
          pkgs.zlib
        ]
        ++ buildTools;

        # Wrapper script that sets up vendor symlinks before running site
        siteWrapper = pkgs.writeShellScriptBin "site" ''
          mkdir -p static/vendor
          ln -sfn ${katexDist} static/vendor/katex
          ln -sfn ${lucideFont} static/vendor/lucide
          exec ${siteBuilder}/bin/site "$@"
        '';

      in
      {
        packages = {
          default = website;
          builder = siteBuilder;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = siteWrapper;
          exePath = "/bin/site";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [ ghcWithDeps ] ++ devTools;

          KATEX_VERSION = pkgs.nodePackages.katex.version or "unknown";
          MERMAID_VERSION = pkgs.mermaid-cli.version or "unknown";

          # Symlink vendor assets into static/ for development
          shellHook = ''
            mkdir -p static/vendor
            ln -sfn ${katexDist} static/vendor/katex
            ln -sfn ${lucideFont} static/vendor/lucide
          '';
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
          text = pkgs.lib.getExe pkgs.treefmt;
        };
      }
    );
}
