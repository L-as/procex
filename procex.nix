{ mkDerivation
, async
, base
, bytestring
, containers
, deepseq
, hspec
, lib
, unix
, utf8-string
}:
mkDerivation {
  pname = "procex";
  version = "0.3.3";
  src = ./.;
  libraryHaskellDepends = [
    async
    base
    bytestring
    containers
    deepseq
    unix
    utf8-string
  ];
  testHaskellDepends = [ async base bytestring hspec unix ];
  description = "Ergonomic process launching with extreme flexibility and speed";
  license = lib.licenses.mit;
}
