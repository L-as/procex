{ mkDerivation, async, base, bytestring, containers, lib, unix
, utf8-string, replace-megaparsec, directory, deepseq, hspec
}:
mkDerivation {
  pname = "procex";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  libraryHaskellDepends = [
    async base bytestring containers unix utf8-string deepseq
  ];
  testHaskellDepends = [
    async base replace-megaparsec unix directory hspec
  ];
  license = lib.licenses.asl20;
  hydraPlatforms = lib.platforms.none;
  doHaddock = false;
  doCheck = false;
}
