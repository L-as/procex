{ pkgs ? import <nixpkgs> {} }:
with pkgs;
let
  mkDerivation = p: old: arg: old (arg // {libraryHaskellDepends = arg.libraryHaskellDepends ++ arg.testHaskellDepends;});
in haskell.packages.ghc922.shellFor {
  packages = p: [
    ((p.callPackage ./procex.nix {}).override (o: {mkDerivation = mkDerivation p o.mkDerivation;}))
  ];
  buildHoogle = false;
  buildInputs = [ cabal-install cabal2nix curl ];
}
