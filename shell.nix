{ pkgs ? import (builtins.storePath /nix/store/sjmq1gphj1arbzf4aqqnygd9pf4hkfkf-source) {} }:
with pkgs;
let hspkgs = haskell.packages.ghc924; in
hspkgs.shellFor {
  packages = p: [
    (p.callPackage ./procex.nix { hspec = p.hspec_2_10_0_1; })
  ];
  buildHoogle = false;
  buildInputs = [ cabal-install cabal2nix curl ];
}
