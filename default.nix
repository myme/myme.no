with import <nixpkgs> {};
let
  srcs = nix-gitignore.gitignoreSourcePure ./.gitignore ./.;
  drv = haskellPackages.callCabal2nix "myme.no" srcs {};
in
  if lib.inNixShell then drv.env else drv
