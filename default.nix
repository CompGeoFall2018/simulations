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
  pkgs = import pinnedPkgs {};

  our-haskell-packages = pkgs.haskell.packages.ghcjs.override {
    overrides = newPkgs: oldPkgs: {
      tasty-quickcheck = (pkgs.haskell.lib.dontCheck (oldPkgs.tasty-quickcheck));
      arithmoi = (pkgs.haskell.lib.dontCheck (oldPkgs.arithmoi));
      scientific = (pkgs.haskell.lib.dontCheck (oldPkgs.scientific));
    };
  };
in
  {
    simulations = our-haskell-packages.callPackage ./simulations.nix {};
  }
