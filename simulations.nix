{ mkDerivation, ghcjs-base, constructible, stdenv }:
mkDerivation {
  pname = "simulations";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ ghcjs-base constructible ];
  executableSystemDepends = [ ];
  license = stdenv.lib.licenses.unfree;
}
