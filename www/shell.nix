with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hakyll, stdenv }:
             mkDerivation {
               pname = "bsima";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [ base hakyll ];
               homepage = "www.bsima.me";
               description = "My website";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
