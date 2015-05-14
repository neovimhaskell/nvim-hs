#!/bin/sh

# Shell script that starts a plugin provider which is interpreted from the
# TestPlugins.hs file inside this repository.

# XXX Should we do more sanity checking here?
# (e.g. test whether cabal is on the PATH)

if [ -d ".cabal-sandbox/" ] ; then
    # Use `cabal exec` if we are in a sandbox
    exec cabal exec runhaskell TestPlugins.hs
else
    exec runhaskell TestPlugins.hs
fi

