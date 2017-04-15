#!/bin/bash

HC=$1

set -ev

if [ -f configure.ac ]; then autoreconf -i; fi
rm -rf dist/
cabal sdist # test that a source-distribution can be generated
cd dist/
SRCTAR=$(ls *.tar.gz)
SRC_BASENAME="${SRCTAR/%.tar.gz}"
tar -xvf "./$SRC_BASENAME.tar.gz"
cd "$SRC_BASENAME/"
## from here on, CWD is inside the extracted source-tarball
rm -fv cabal.project.local
echo 'packages: .' > cabal.project

for apiblob in ../../apiblobs/*.msgpack ; do
    ## since we don't have neovim installed, use a file for the api genneration
    echo "Testing with neovim api version ${apiblob}"
    ln -fs "${apiblob}" api
    rm -f cabal.project.freeze
    cabal new-build -w ${HC} --disable-tests --disable-benchmarks all
    ls
    cabal new-build -w ${HC} ${TEST} ${BENCH} all
    if [ "x$TEST" = "x--enable-tests" ]; then
        cabal new-test -w ${HC} ${TEST} all
    fi;
done

