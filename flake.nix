{
  description = "Flake for myme.no";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";

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

      packages.${system} = pkgs.myme-site;

      defaultPackage.${system} = self.packages.${system}.image;

      devShells.${system} = {
        # Development shell for the webpage (./site)
        site = pkgs.mkShell {
          buildInputs = [ ssg ];
        };

        # Development shell for the static site genrator (./ssg)
        ssg = pkgs.haskellPackages.shellFor {
          packages = ps: [ ssg ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            ghcid
            hlint
          ];
        };
      };

      devShell.${system} = self.devShells.${system}.site;
    };
}
