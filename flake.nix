{
  description = "Conjure: the automated constraint modelling tool";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      # The Makefile's default GHC_VERSION, and the version CI runs the tests with.
      ghcVersion = "ghc910";

      # The string etc/build/version.sh would produce (`git log -1 --pretty=format:"%h (%ai)"`).
      # A Nix build has no .git directory, so it comes from the flake's metadata instead.
      repositoryVersion =
        let
          rev = self.shortRev or self.dirtyShortRev or "unknown";
          date = self.lastModifiedDate or "19700101000000";
          part = start: len: builtins.substring start len date;
        in
        "${rev} (${part 0 4}-${part 4 2}-${part 6 2} ${part 8 2}:${part 10 2}:${part 12 2} +0000)";

      # Only the files the Haskell build reads, so changes to tests/ or docs/ don't trigger a rebuild.
      haskellSrc = lib.fileset.toSource {
        root = ./.;
        fileset = lib.fileset.unions [
          ./conjure-cp.cabal
          ./LICENSE
          ./src
          ./etc/build/gen_Expression.hs
          ./etc/build/gen_Operator.hs
        ];
      };

      forAllSystems =
        f:
        lib.genAttrs systems (
          system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            hlib = pkgs.haskell.lib.compose;

            hsPkgs = pkgs.haskell.packages.${ghcVersion}.override (old: {
              overrides = lib.composeExtensions (old.overrides or (_: _: { })) (
                hfinal: _: {
                  conjure-cp = hlib.overrideCabal (drv: {
                    # Generate the gitignored modules that `make preinstall` normally creates.
                    preConfigure = (drv.preConfigure or "") + ''
                      cat > src/Conjure/RepositoryVersion.hs <<'EOF'
                      module Conjure.RepositoryVersion where
                      import Prelude
                      repositoryVersion :: String
                      repositoryVersion = "${repositoryVersion}"
                      EOF
                      runghc etc/build/gen_Operator.hs
                      runghc etc/build/gen_Expression.hs
                    '';
                  }) (hfinal.callCabal2nix "conjure-cp" haskellSrc { });
                }
              );
            });

            # The Savile Row bundled in etc/savilerow, which `make install` also installs.
            savilerow = pkgs.stdenvNoCC.mkDerivation {
              name = "savilerow";
              src = ./etc/savilerow;
              nativeBuildInputs = [ pkgs.makeWrapper ];
              dontConfigure = true;
              dontBuild = true;
              installPhase = ''
                runHook preInstall
                mkdir -p $out/bin $out/libexec
                cp -r . $out/libexec/savilerow
                chmod +x $out/libexec/savilerow/savilerow
                makeWrapper $out/libexec/savilerow/savilerow $out/bin/savilerow \
                  --prefix PATH : ${lib.makeBinPath [ pkgs.jre_headless ]}
                runHook postInstall
              '';
              meta = {
                description = "Savile Row constraint modelling assistant, as bundled with Conjure";
                homepage = "https://savilerow.cs.st-andrews.ac.uk";
                mainProgram = "savilerow";
              };
            };

            conjure-unwrapped = lib.pipe hsPkgs.conjure-cp [
              hlib.dontCheck # the test suite needs external solvers
              hlib.justStaticExecutables
            ];

            # conjure together with the bundled savilerow, like `make install`.
            conjure = pkgs.symlinkJoin {
              name = "conjure-${conjure-unwrapped.version}";
              paths = [
                conjure-unwrapped
                savilerow
              ];
              nativeBuildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/conjure --prefix PATH : ${savilerow}/bin
              '';
              meta = {
                inherit (conjure-unwrapped.meta) description homepage license;
                mainProgram = "conjure";
              };
            };
          in
          f {
            inherit
              pkgs
              hsPkgs
              savilerow
              conjure
              ;
          }
        );
    in
    {
      packages = forAllSystems (
        { conjure, savilerow, ... }:
        {
          inherit conjure savilerow;
          default = conjure;
        }
      );

      devShells = forAllSystems (
        {
          pkgs,
          hsPkgs,
          savilerow,
          ...
        }:
        {
          default = hsPkgs.shellFor {
            packages = p: [ p.conjure-cp ];
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.hlint
              hsPkgs.haskell-language-server
              savilerow
            ];
            shellHook = ''
              # Generate the gitignored modules that `make preinstall` creates, so `cabal build` works straight away.
              if [ -f conjure-cp.cabal ]; then
                bash etc/build/version.sh > /dev/null
                runghc etc/build/gen_Operator.hs > /dev/null
                runghc etc/build/gen_Expression.hs > /dev/null
              fi
            '';
          };
        }
      );
    };
}
