let
  dontCheckPackages = [
    "optparse-applicative"
    "arithmoi"
  ];

  # Jailbreak these packages
  doJailbreakPackages = [
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  pinnedPkgs = builtins.fetchTarball {
    sha256 = "1x9zjpzijj5sx2hxdsdhm4fi5m22x9n2gp25wb71p1m8wfzqwrib";
    url = "https://github.com/NixOS/nixpkgs/archive/aed2dea55a9399c9513e65ff8248aff0205b5a3e.tar.gz";
  };
  mkPkgs = opts: import pinnedPkgs opts;
  pkgs0 = mkPkgs {};
  ghc = pkgs0.haskell.compiler.ghcjs84;
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
