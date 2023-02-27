{ mkDerivation
, lib
, base
, exceptions
, stm
, time
, transformers
}:
mkDerivation {
  pname = "cached-io";
  version = "1.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    exceptions
    stm
    time
    transformers
  ];
  description = "A simple library to cache a single IO action with timeout";
  license = lib.licenses.asl20;
}
