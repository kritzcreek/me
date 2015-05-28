# [www.bsima.me](www.bsima.me)

Requires [Haskell and Cabal setup with Nix](http://stackoverflow.com/questions/27968909/how-to-get-cabal-and-nix-work-together)

Build like so:

   make configure
   make build
   make site
   make deploy

If you change `bsima.cabal`, you'll need to regenerate `shell.nix`:

   make nix

Than build all over again.
