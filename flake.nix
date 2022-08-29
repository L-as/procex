{
  description = "procex";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
  };
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};
      hsOverlay = hsPkgs: hsPkgs.override {
        overrides = final: prev: {
          procex = final.callPackage ./procex.nix { hspec = final.hspec_2_10_0_1; };
        };
      };
      hsPkgsFor = system: hsOverlay (pkgsFor system).haskell.packages.ghc924;
      formattersFor = system: with (pkgsFor system); [
        nixpkgs-fmt
        haskellPackages.cabal-fmt
        (haskell.lib.compose.overrideCabal (_:{doCheck = false;}) (hsPkgsFor system).fourmolu_0_8_0_0)
      ];
      regen = system: (pkgsFor system).writeShellApplication {
        name = "regen";
        runtimeInputs = [ (pkgsFor system).cabal2nix ] ++ formattersFor system;
        text = ''
          set -xe
          cabal2nix ./. > procex.nix
          ./bin/format
        '';
      };
    in
    {
      checks = perSystem (system: {
        formatting = (pkgsFor system).runCommandNoCC "formatting-check"
          {
            nativeBuildInputs = formattersFor system;
          } ''
          cd ${self}
          ./bin/format check
          touch $out
        '';
        cabal2nix = (pkgsFor system).runCommandNoCC "cabal2nix-check"
          {
            nativeBuildInputs = [ (pkgsFor system).cabal2nix ];
          } ''
          cd ${self}
          diff <(cabal2nix ./.) procex.nix
          touch $out
        '';
      });
      apps = perSystem (system: {
        regen.type = "app";
        regen.program = "${regen system}/bin/regen";
      });
      packages = perSystem (system: {
        default = (hsPkgsFor system).procex;
      });
      devShells = perSystem (system: {
        default = (hsPkgsFor system).shellFor {
          packages = p: [ p.procex ];
          buildHoogle = true;
          nativeBuildInputs = with (pkgsFor system); [
            cabal-install
            hlint
            cabal2nix
            curl
          ] ++ formattersFor system;
        };
      });
    };
}
