with import ./nixpkgs.nix {};
let
  drv = import ./default.nix;
in
  haskellPackages.shellFor {
    packages = ps: [ drv ];
    buildInputs = with haskellPackages; [
      cabal-install
      ghcid
      hlint
    ];
  }
