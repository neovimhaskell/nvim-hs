# nvim-hs

Neovim API for Haskell plugins as well as a plugin provider.

[![Build Status](https://travis-ci.org/saep/nvim-hs.svg?branch=master)](https://travis-ci.org/saep/nvim-hs)

# What has been done so far?

* The API offered by [neovim](https://github.com/neovim/neovim) 
  can be used from haskell. The functions are generated via 
  Template Haskell, are named with a `vim_` prefix and its 
  types can be queried inside ghci.
* The msgpack-rpc part is mostly implemented. The purpose 
  of *notifications* is still not clear to us 
  (and they have no yet been encountered yet).
* A sample configuration which tests new features and helps 
  finding issues in our API is hosted here: 
  https://github.com/saep/nvim-hs-config-example

# Coordinate your work

Talk to osa1 or saep on #neovim on freenode!

You can also just create an issue here on github.

# Installation

As this is still very much a prototype and changes are most certainly
necessary, the only recommended way to install this is within a sandbox.
If you encounter any issues while following these steps, please let us
know!

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
