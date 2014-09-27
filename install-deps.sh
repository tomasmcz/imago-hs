#!/bin/bash

cabal sandbox init
cabal install cabal
cabal install wx
cabal install reactive-banana-wx --allow-newer
cabal install repa
cabal install repa-devil --allow-never
cabal install --only-dependencies
