#!/bin/sh

# Shell script that starts a plugin provider which is interpreted from the
# TestPlugins.hs file inside this repository.

if [ -d ".cabal-sandbox/" ] ; then
    if ! type cabal > /dev/null 2>&1 ; then
        echo "cabal-install program not installed or on PATH"
        echo $PATH
        exit 1
    fi

    if ! type runghc > /dev/null 2>&1 ; then
        echo "runghc not installed or on PATH"
        echo $PATH
        exit 2
    fi
    # Use `cabal exec` if we are in a sandbox
    exec cabal exec runghc TestPlugins.hs -- $1 -l test-log.txt -v DEBUG
elif [ -d ".stack-work" ] ; then
    exec stack runghc TestPlugins.hs -- $1 -l test-log.txt -v DEBUG
else
    if ! type runghc > /dev/null 2>&1 ; then
        echo "runghc not installed or on PATH"
        echo $PATH
        exit 3
    fi
    exec runghc TestPlugins.hs -- $1 -l test-log.txt -v DEBUG
fi

