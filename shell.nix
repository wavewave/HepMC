with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, attoparsec, base, blaze-builder, bytestring, pipes
             , pipes-attoparsec, pipes-parse, pipes-text, stdenv, text
             , transformers
             }:
             mkDerivation {
               pname = "HepMC";
               version = "0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 attoparsec base blaze-builder bytestring pipes pipes-attoparsec
                 pipes-parse pipes-text text transformers
               ];
               homepage = "http://github.com/wavewave/HepMC";
               description = "Haskell HepMC parser and builder";
               license = stdenv.lib.licenses.gpl3;
             }) {};
in
  pkg.env
