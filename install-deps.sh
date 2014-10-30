#!/bin/bash

set -e

cabal sandbox init
cabal sandbox add-source ../imago
cabal install Cabal
cabal install wx
cabal install reactive-banana-wx --allow-newer
cabal install repa
cabal install --only-dependencies
