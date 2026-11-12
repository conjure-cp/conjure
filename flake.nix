{
  description = "Conjure: the automated constraint modelling tool";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # Solvers, pinned to the versions in etc/build/install-minion.sh and etc/build/install-chuffed.sh.
    minion = {
      url = "github:minion/minion/5c9f347c864bc2b64684b1df9d366294c21aac49";
      flake = false;
    };
    chuffed = {
      url = "github:chuffed/chuffed/0.13.2";
      flake = false;
    };
  };

  outputs =
    { self, nixpkgs, ... }@inputs:
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

      # Formats a flake's lastModifiedDate ("YYYYMMDDHHMMSS") as "YYYY-MM-DD".
      isoDate =
        d: "${builtins.substring 0 4 d}-${builtins.substring 4 2 d}-${builtins.substring 6 2 d}";

      # The string etc/build/version.sh would produce (`git log -1 --pretty=format:"%h (%ai)"`).
      # A Nix build has no .git directory, so it comes from the flake's metadata instead.
      repositoryVersion =
        let
          rev = self.shortRev or self.dirtyShortRev or "unknown";
          d = self.lastModifiedDate or "19700101000000";
          time = "${builtins.substring 8 2 d}:${builtins.substring 10 2 d}:${builtins.substring 12 2 d}";
        in
        "${rev} (${isoDate d} ${time} +0000)";

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

            # Built the same way as etc/build/install-minion.sh.
            minion = pkgs.stdenv.mkDerivation {
              pname = "minion";
              version = "0-unstable-${isoDate inputs.minion.lastModifiedDate}";
              src = inputs.minion;
              nativeBuildInputs = [ pkgs.python3 ];
              configurePhase = ''
                runHook preConfigure
                mkdir build
                cd build
                python3 ../configure.py --compiler c++
                runHook postConfigure
              '';
              buildFlags = [ "minion" ];
              enableParallelBuilding = true;
              installPhase = ''
                runHook preInstall
                install -Dm755 minion $out/bin/minion
                runHook postInstall
              '';
              meta = {
                description = "Minion constraint solver";
                homepage = "https://github.com/minion/minion";
                mainProgram = "minion";
              };
            };

            chuffed = pkgs.stdenv.mkDerivation {
              pname = "chuffed";
              version = "0.13.2"; # keep in step with the chuffed input
              src = inputs.chuffed;
              nativeBuildInputs = [ pkgs.cmake ];
              meta = {
                description = "Chuffed lazy clause generation constraint solver";
                homepage = "https://github.com/chuffed/chuffed";
                mainProgram = "fzn-chuffed";
              };
            };

            conjure-unwrapped = lib.pipe hsPkgs.conjure-cp [
              hlib.dontCheck # the test suite needs external solvers
              hlib.justStaticExecutables
            ];

            # conjure plus the bundled savilerow, like `make install`. conjure's PATH also gets
            # savilerow, minion and chuffed, so `conjure solve` works without installing anything else.
            conjure = pkgs.symlinkJoin {
              name = "conjure-${conjure-unwrapped.version}";
              paths = [
                conjure-unwrapped
                savilerow
              ];
              nativeBuildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/conjure \
                  --prefix PATH : ${
                    lib.makeBinPath [
                      savilerow
                      minion
                      chuffed
                    ]
                  }
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
              minion
              chuffed
              conjure
              ;
          }
        );
    in
    {
      packages = forAllSystems (
        {
          conjure,
          savilerow,
          minion,
          chuffed,
          ...
        }:
        {
          inherit
            conjure
            savilerow
            minion
            chuffed
            ;
          default = conjure;
        }
      );

      devShells = forAllSystems (
        {
          pkgs,
          hsPkgs,
          savilerow,
          minion,
          chuffed,
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
              minion
              chuffed
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

      # The "Simple test" from .github/workflows/build.yml, run against the Nix build.
      checks = forAllSystems (
        { pkgs, conjure, ... }:
        {
          solve = pkgs.runCommand "conjure-solve-test" { nativeBuildInputs = [ conjure ]; } ''
            export HOME=$TMPDIR
            echo "find x : set of int(1..5) such that sum(x)=5" > test.essence
            conjure solve test.essence --number-of-solutions=all --solutions-in-one-file --output-format=json
            echo '[{"x":[5]},{"x":[2,3]},{"x":[1,4]}]' | tr -d '[:space:]' > expected
            tr -d '[:space:]' < test.solutions.json > actual
            diff expected actual
            touch $out
          '';
        }
      );
    };
}
