# nvim-hs

Neovim API for Haskell plugins as well as the plugin provider

# What has been done so far?

* The code can be generated, although the generated code may change if the
  need arises.
* The msgpack-rpc is partly implemented. It is possible to send requests to
  a neovim instance and receive the corresponding reply.

# Coordinate your work

Currently I am still all alone on this project. If you want to work on
something or if you have any questions, contact me. You can create a pull
request/issue with what you want to do and I will (usually) comment within a
day. I'm also on IRC (e.g. #neovim, #haskell on freenode) with the nick
_saep_. When I'm actually online (i.e. I am connected to my bouncer), I
should respond within 5 minutes if you highlight me or write a personal
message.

# Installation

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

