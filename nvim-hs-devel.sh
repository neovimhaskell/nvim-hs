#!/bin/sh

# Helper script to run nvim-hs in a development environment.

# You should not name this script nvim-hs and put it on your path.
if [ x"$0" = x`which nvim-hs` ] ; then
    echo "This script has the potential to run in an infinite loop. Exiting now..."
    exit 1
fi

# This variable defines where the sandbox or stack files can be found. Adjust it
# appropriately and then delete the line after it.
sandbox_directory=$HOME/git/nvim-hs
echo "I should have read the comments. Silly me." && exit 1

old_pwd="`pwd`"
cd "$sandbox_directory"

if [ -d "$sandbox_directory/.cabal-sandbox" ] ; then
    # We detect the sandbox by checking for the directory .cabal-sandbox
    # This should work most of the time.
    env CABAL_SANDBOX_CONFIG="$sandbox_directory"/cabal.sandbox.config cabal \
        exec "$sandbox_directory/.cabal-sandbox/bin/nvim-hs" -- "$@"
elif [ -d "$sandbox_directory/.stack-work" ] ; then
    # Stack leaves behind a .stack-work directory, so we take its present as a
    # sign to use this approach.
    PATH=`stack path --bin-path` stack exec nvim-hs -- "$@"
else
    echo "No development directories found. Have you built the project?"
    exit 2
fi
cd "$old_pwd"

# vim: foldmethod=marker sts=2 ts=4 expandtab sw=4
