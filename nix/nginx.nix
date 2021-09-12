{ dockerTools
, nginx
, runCommand
, symlinkJoin
, writeText
, writeTextDir
, nginxWebRoot
, nginxPort ? "80"
, imageName ?  "nginx-container"
}:

let
  nginxConf = writeText "nginx.conf" ''
    user nobody nobody;
    daemon off;
    error_log /dev/stdout info;
    pid /dev/null;
    events {}
    http {
      include ${nginx}/conf/mime.types;
      access_log /dev/stdout;
      server {
        listen ${nginxPort};
        index index.html;
        location / {
          root ${nginxWebRoot};
        }
      }
    }
  '';

  # Provide a /etc/passwd and /etc/group that contain root and nobody.
  # Useful when packaging binaries that insist on using nss to look up
  # username/groups (like nginx).
  # https://github.com/NixOS/nixpkgs/blob/b5f2c5f132e05537330f1ba668e57e809a2e1ad6/pkgs/build-support/docker/default.nix#L742
  fakeNss = symlinkJoin {
    name = "fake-nss";
    paths = [
      (writeTextDir "etc/passwd" ''
        root:x:0:0:root user:/var/empty:/bin/sh
        nobody:x:65534:65534:nobody:/var/empty:/bin/sh
      '')
      (writeTextDir "etc/group" ''
        root:x:0:
        nobody:x:65534:
      '')
      (writeTextDir "etc/nsswitch.conf" ''
        hosts: files dns
      '')
      (runCommand "var-empty" { } ''
        mkdir -p $out/var/empty
      '')
    ];
  };

in dockerTools.buildLayeredImage {
  name = imageName;
  tag = "latest";
  contents = [ fakeNss nginx ];

  extraCommands = ''
    # nginx still tries to read this directory even if error_log
    # directive is specifying another file :/
    mkdir -p var/log/nginx
    mkdir -p var/cache/nginx
  '';

  config = {
    Cmd = [ "nginx" "-c" nginxConf ];
    ExposedPorts = {
      "${nginxPort}/tcp" = {};
    };
  };
}
