{
  description = "codgician's personal homepage.";

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
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true; # Required for google-chrome on aarch64-darwin
        };
        lib = pkgs.lib;
        hPkgs = pkgs.haskellPackages;

        # Local packages (manually packaged dependencies)
        localPkgs = import ./packages { inherit pkgs; };

        # Source files for the site builder
        builderSrc = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            ./src
            ./app
            ./test
            ./package.yaml
            ./builder.cabal
            ./LICENSE
          ];
        };

        # The Hakyll site generator (with tests)
        siteBuilder = hPkgs.callCabal2nix "builder" builderSrc { };

        # Browser for mermaid-cli (platform-aware)
        browser =
          if pkgs.stdenv.isDarwin then
            pkgs.google-chrome
          else
            pkgs.chromium;
        browserPath =
          if pkgs.stdenv.isDarwin then
            "${browser}/bin/google-chrome-stable"
          else
            "${lib.getExe browser}";

        # External tools needed for building
        buildTools = [
          pkgs.dart-sass
          pkgs.nodePackages.katex
          pkgs.mermaid-cli
          browser
        ];

        # KaTeX dist path for CSS and fonts
        katexDist = "${pkgs.nodePackages.katex}/lib/node_modules/katex/dist";

        # Lucide icons (icon font with CSS)
        lucideFont = "${localPkgs.lucide-static}/font";

        # Reveal.js for slides
        revealJs = localPkgs.reveal-js;

        # Tool versions for cache key (required by site builder)
        katexVersion = pkgs.nodePackages.katex.version;
        mermaidVersion = pkgs.mermaid-cli.version;

        # The final website derivation
        website = pkgs.stdenv.mkDerivation {
          name = "codgician-site";
          src = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./content
              ./templates
              ./static
              ./config.yaml
              ./puppeteer-config.json
            ];
          };
          nativeBuildInputs = [ siteBuilder ] ++ buildTools;

          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = lib.optionalString (
            pkgs.stdenv.hostPlatform.libc == "glibc"
          ) "${pkgs.glibcLocales}/lib/locale/locale-archive";

          KATEX_VERSION = katexVersion;
          MERMAID_VERSION = mermaidVersion;
          PUPPETEER_EXECUTABLE_PATH = browserPath;
          PUPPETEER_CONFIG = "./puppeteer-config.json";

          # Chromium crashpad workaround for sandboxed builds (Linux CI)
          XDG_CONFIG_HOME = "/tmp/.chromium";
          XDG_CACHE_HOME = "/tmp/.chromium";


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

            # Copy reveal.js for slides
            mkdir -p static/vendor/reveal.js
            cp -r ${revealJs}/dist static/vendor/reveal.js/
            cp -r ${revealJs}/plugin static/vendor/reveal.js/

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
          # Test dependencies
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
        ] ++ buildTools;

        # Wrapper script that sets up vendor symlinks before running site
        siteWrapper = pkgs.writeShellScriptBin "site" ''
          mkdir -p static/vendor
          ln -sfn ${katexDist} static/vendor/katex
          ln -sfn ${lucideFont} static/vendor/lucide
          ln -sfn ${revealJs} static/vendor/reveal.js
          export KATEX_VERSION="${katexVersion}"
          export MERMAID_VERSION="${mermaidVersion}"
          export PUPPETEER_EXECUTABLE_PATH="${browserPath}"
          export PUPPETEER_CONFIG="./puppeteer-config.json"
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

          KATEX_VERSION = katexVersion;
          MERMAID_VERSION = mermaidVersion;
          PUPPETEER_EXECUTABLE_PATH = browserPath;
          PUPPETEER_CONFIG = "./puppeteer-config.json";

          # Symlink vendor assets into static/ for development
          shellHook = ''
            mkdir -p static/vendor
            ln -sfn ${katexDist} static/vendor/katex
            ln -sfn ${lucideFont} static/vendor/lucide
            ln -sfn ${revealJs} static/vendor/reveal.js
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
