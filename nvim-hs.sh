#!/bin/sh

# Shell script that starts a plugin provider which is interpreted from the
# TestPlugins.hs file inside this repository.

if ! type cabal > /dev/null 2>&1 ; then
    echo "cabal not installed or on PATH"
    echo $PATH
    exit 1
fi

if ! type runghc > /dev/null 2>&1 ; then
    echo "runghc not installed or on PATH"
    echo $PATH
    exit 1
fi

if [ -d ".cabal-sandbox/" ] ; then
    # Use `cabal exec` if we are in a sandbox
    exec cabal exec runghc TestPlugins.hs
else
    exec runghc TestPlugins.hs
fi

