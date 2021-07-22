# Copy this directory and put it in your environment.systemPackages as so: `import ./some/path/myshell/default.nix { inherit pkgs; }`
# Make sure to change shellrcSrc and shellrcModule.

{ pkgs ? import <nixpkgs> {} }:

let
  # NB: You must change this.
  shellrcSrcPath = "/the/absolute/path/to/this/folder";
  shellrcModule = "ShellRC";

  shellrcSrc = /. + shellrcSrcPath;
  shellrcModulePath = builtins.replaceStrings ["."] ["/"] shellrcModule + ".hs";

  procex = p:
    let
      rev = "079450b3447ce0669aea64ecca5b84ff2024272d"; # Update this whenever you feel like it. Eventually you can just replace this with `p.procex`.
      sha256 = "07c50agsn54jay8x5jk8rijksgzaj2prigmasc1f1x0yvshsniyd";
      src = builtins.fetchTarball {
        url = "https://github.com/L-as/procex/archive/${rev}.tar.gz";
        inherit sha256;
      };
    in p.callPackage "${src}/procex.nix" {};
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [p.utf8-string p.async p.replace-megaparsec p.pretty-simple (procex p)]);
  args = builtins.concatStringsSep " " [
    "-XExtendedDefaultRules"
    "-XOverloadedStrings"
    "-Wall"
    "-Wno-type-defaults"
  ];

  shellrc = pkgs.runCommand "shellrc" {} ''
    cp ${shellrcSrc} --no-preserve=all -rT $out
    ${ghc}/bin/ghc -c -dynamic --make -i"$out" ${args} $out/${shellrcModulePath}
  '';
  init = pkgs.runCommand "ghci-init" {} ''
    cat > $out <<END
      :set +m

      :l ${shellrcModule}

      import qualified Procex.Shell

      Procex.Shell.initInteractive

      :set prompt-function promptFunction

      :def! li (\_ -> pure ":set -i\n:set -i${shellrcSrcPath}\n:load ${shellrcModule}")
    END
    grep -E '^import .*$' < ${shellrcSrc}/${shellrcModulePath} >> $out
  '';
in pkgs.writeShellScriptBin "s" ''
exec env GHCRTS="-c" ${ghc}/bin/ghci ${args} -ignore-dot-ghci -interactive-print Text.Pretty.Simple.pPrint -i -i${shellrc} -ghci-script ${init} "$@"
''
