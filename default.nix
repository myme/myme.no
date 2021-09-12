{ pkgs ? import ./nix/nixpkgs.nix {} }:

let
  ssg = pkgs.callPackage ./ssg {};
  site = pkgs.callPackage ./site { inherit ssg; };
  nginx = pkgs.callPackage ./nix/nginx.nix {
    imageName = "myme.no";
    nginxWebRoot = site;
  };

in nginx
