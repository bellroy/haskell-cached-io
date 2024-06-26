{ mkDerivation, base, exceptions, lib, stm, time, transformers }:
mkDerivation {
  pname = "cached-io";
  version = "1.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base exceptions stm time transformers ];
  executableHaskellDepends = [ base ];
  description = "A simple library to cache IO actions";
  license = lib.licenses.asl20;
  mainProgram = "test-cachedIO";
}
