{ pkgs ? import ./nix/nixpkgs.nix {} }:
let
  ssg = pkgs.callPackage ./ssg {};
in
  pkgs.haskellPackages.shellFor {
    packages = ps: [ ssg ];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      hlint
      ssg
    ];
  }
