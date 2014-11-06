#!/bin/bash

set -ex

cabal sandbox init
cabal sandbox add-source ../imago
cabal install Cabal $1
cabal install wx $1
cabal install reactive-banana-wx --allow-newer $1
cabal install repa $1
cabal install --only-dependencies $1
