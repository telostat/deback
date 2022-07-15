{ mkDerivation, aeson, ansi-terminal, base, bytestring, lib
, optparse-applicative, table-layout, text, typed-process
}:
mkDerivation {
  pname = "deback";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal base bytestring optparse-applicative
    table-layout text typed-process
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
