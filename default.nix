{ pkgs ? import ./nix/nixpkgs.nix {} }:

let
  ssg = pkgs.callPackage ./ssg {};
  site = pkgs.callPackage ./site { inherit ssg; };
  image = pkgs.callPackage ./nix/image.nix {
    imageName = "myme.no";
    nginxWebRoot = site;
  };

in image
