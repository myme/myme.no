{
  description = "Flake for myme.no";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      };
      ssg = pkgs.myme-site.ssg;
    in {
      overlay = (final: parent: {
        myme-site = parent.callPackage ./. { };
      });

      defaultPackage.${system} = pkgs.myme-site.image;

      devShell.${system} = pkgs.haskellPackages.shellFor {
        packages = ps: [ ssg ];
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          ghcid
          hlint
          ssg
        ];
      };
    };
}
