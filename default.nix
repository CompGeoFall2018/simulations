let
  dontCheckPackages = [
    "optparse-applicative"
    "arithmoi"
  ];

  # Jailbreak these packages
  doJailbreakPackages = [
    "turtle"
    "haddock-library-ghcjs"
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  mkPkgs = opts: import <unstable> opts;
  ghc = (mkPkgs {}).haskell.compiler.ghcjs84;
  # ghc = (mkPkgs {}).ghc843;

  config = {
    allowUnfree = true;

    packageOverrides = pkgs: rec {
      haskellPackages =
        let
            
          generatedOverrides = haskellPackagesNew: haskellPackagesOld:
            { simulations = haskellPackagesNew.callPackage ./simulations.nix {}; };

          makeOverrides =
            function: names: haskellPackagesNew: haskellPackagesOld:
              let
                toPackage = name: {
                  inherit name;

                  value = function haskellPackagesOld.${name};
                };

            in
              builtins.listToAttrs (map toPackage names);

          composeExtensionsList =
            pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

          # More exotic overrides go here
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          };
        in
          pkgs.haskellPackages.override {
            inherit ghc;
            overrides = composeExtensionsList [
              generatedOverrides
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              manualOverrides
            ];
          };
    };
  };

  pkgs = mkPkgs { inherit config; };

  /*
let
  withDeps = ghc: ghc.withHoogle (pkgs: [ pkgs.constructible ]);
  #haskell = ghc.withHoogle (pkgs: [ pkgs.constructible ]);
  ghc-with-deps = withDeps haskell.compiler.ghcjs;
in
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    ghc-with-deps cabal-install
    # haskellPackages.ghcjs-base
  ];
}
*/

in
  rec {
    simulations = pkgs.haskellPackages.simulations;
  }
