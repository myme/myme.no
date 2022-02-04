{ callPackage }:
rec {
  ssg = callPackage ./ssg {};
  site = callPackage ./site { inherit ssg; };
  image = callPackage ./image.nix {
    imageName = "myme.no";
    nginxWebRoot = site;
  };
}
