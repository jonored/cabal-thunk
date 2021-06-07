{ mkDerivation, base, bytestring, Cabal, directory
, optparse-applicative, process, stdenv, strict, text
}:
mkDerivation {
  pname = "cabal-thunk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring Cabal directory optparse-applicative process strict
    text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
