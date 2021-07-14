{ mkDerivation, async, base, bytestring, containers, lib, unix
, utf8-string
}:
mkDerivation {
  pname = "las";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base bytestring containers unix utf8-string
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
  doHaddock = false;
  doCheck = false;
}
