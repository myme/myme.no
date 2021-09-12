{ glibcLocales, nix-gitignore, ssg, stdenv }:
stdenv.mkDerivation {
  name = "myme.no-site";
  version = "0.1.0";
  srcs = nix-gitignore.gitignoreSourcePure ../.gitignore ./.;
  buildInputs = [
    glibcLocales
  ];
  LANG="en_US.UTF-8";
  buildPhase = ''
    ${ssg}/bin/ssg build
  '';
  installPhase = ''
    cp -av public $out
  '';
}
