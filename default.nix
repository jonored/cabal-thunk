{pkgs ? import <nixpkgs> {}}:

pkgs.haskellPackages.callPackage ./cabal-thunk.nix { }
