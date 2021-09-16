# myme.no

Main blog on https://myme.no

## Building `myme.no`

`nix-build`

## Building static pages

`cabal run myme.no -- build`

## Run development server

`cabal run myme.no -- watch`

## Publishing

`docker load < result`
