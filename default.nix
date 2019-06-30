with import <nixpkgs> {};
let
  srcs = nix-gitignore.gitignoreSourcePure ./.gitignore ./.;
in
  haskellPackages.callCabal2nix "myme.no" srcs {}
