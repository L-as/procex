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
      rev = "98a207dc6558a428304d58b78bfb8e812885947a"; # 0.2.3
      sha256 = "040n64rf1nirv2l8rlcq06zqp1rdn5ll2yqlyc699d1ah1mlxkdv";
      src = builtins.fetchTarball {
        url = "https://github.com/L-as/procex/archive/${rev}.tar.gz";
        inherit sha256;
      };
    in p.callPackage "${src}/procex.nix" {};
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [p.utf8-string p.async p.replace-megaparsec p.pretty-simple (procex p)]);
  args = builtins.concatStringsSep " " [
    "-XExtendedDefaultRules"
    "-XOverloadedStrings"
    "-XOverloadedLabels"
    "-Wall"
    "-Wno-type-defaults"
  ];

  shellrc = pkgs.runCommand "shellrc" {} ''
    cp ${shellrcSrc} --no-preserve=all -rT $out
    ${ghc}/bin/ghc -c -dynamic --make -i"$out" ${args} $out/${shellrcModulePath}
  '';
  init = pkgs.runCommand "ghci-init" {} ''
    cat > $out <<END
      :set +m -interactive-print Text.Pretty.Simple.pPrint

      :l ${shellrcModule}

      import Procex.Shell.Labels

      :set prompt-function promptFunction

      :def! li (\_ -> pure ":set -i\n:set -i${shellrcSrcPath}\n:load ${shellrcModule}")

      _init
    END
    grep -E '^import .*$' < ${shellrcSrc}/${shellrcModulePath} >> $out
  '';
in pkgs.writeShellScriptBin "s" ''
# This is necessary because otherwise GHCi will load packages
# installed by cabal...
home="$HOME/.local/share/ghci-shell"
mkdir -p "$home"
exec env GHCRTS="-c" HOME="$home" REALHOME="$HOME" ${ghc}/bin/ghci ${args} -ignore-dot-ghci -i -i${shellrc} -ghci-script ${init} "$@"
''
