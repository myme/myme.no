# myme.no

Main blog on https://myme.no

## Build `myme.no` Docker image

```sh
nix build
```

Note: Development is intended to use `nix flakes`.

## Build static site only

```sh
nix build .#site
```

## Writing posts (& frontend work)

```sh
nix develop
cd site
ssg watch
# hack hack hack
```

## Working on the static site generator

```sh
nix develop .#ssg
cd ssg
# hack hack hack
```

