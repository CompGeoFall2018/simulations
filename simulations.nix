{ mkDerivation, base, constructible, stdenv, tar }:
mkDerivation {
  pname = "simulations";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base constructible ];
  executableSystemDepends = [ ];
  license = stdenv.lib.licenses.unfree;
}
