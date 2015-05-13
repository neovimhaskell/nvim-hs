# nvim-hs

Neovim API for Haskell plugins as well as a plugin provider.

[![Build Status](https://travis-ci.org/saep/nvim-hs.svg?branch=master)](https://travis-ci.org/saep/nvim-hs)

# What has been done so far?

* The code can be generated, although the generated code may change if the
  need arises.
* The msgpack-rpc is partly implemented. It is possible to send requests to
  a neovim instance and receive the corresponding reply.

# Coordinate your work

Talk to osa1 or saep on #neovim on freenode!

You can also just create an issue here on github.

# Installation

As this is still very much a prototype and changes are most certainly
necessary, the only recommended way to install this is within a sandbox.

## Sandbox

1. Make sure that the `nvim` executable is on your path
2. `cd` to the project directory (e.g. `~/sandboxes/nvim-hs`)
3. Initialize the sandbox with `cabal sandbox init`
4. Install to the sandbox `cabal install`
5. Create this script
  ```bash
  #!/bin/sh

  sandbox_directory=$HOME/sandboxes/nvim-hs
  old_pwd="`pwd`"
  cd "$sandbox_directory"
  env CABAL_SANDBOX_CONFIG="$sandbox_directory"/cabal.sandbox.config cabal \
      exec "$sandbox_directory/.cabal-sandbox/bin/nvim-hs" -- "$@"
  cd "$old_pwd"
  ```
6. Make the script executable and available from your $PATH

