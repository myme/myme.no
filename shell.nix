{ pkgs ? import ./nix/nixpkgs.nix {} }:
let
  ssg = pkgs.callPackage ./ssg {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      ghcid
      hlint
      ssg
    ];
  }
