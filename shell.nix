{ pkgs ? import <nixpkgs> {} }:
with pkgs;
haskellPackages.shellFor {
  packages = p: [(p.callPackage ./procex.nix {})];
  buildHoogle = false;
  buildInputs = [cabal-install cabal2nix];
}
