# nvim-hs

Neovim API for Haskell plugins as well as a plugin provider. 
This library and executable should provide a basis for developing
plugins. This package should only contain broadly useful interfaces
to write plugins for neovim in haskell. The design goal was to create
an easy to use API that avoids most of the boilerplate while still retaining
some sense of reliability and type safety. Since Template Haskell is used
to generate the neovim bindings and to avoid some of the boilerplate
handy work, some exotic operating systems and architectures may not work.

[![Build Status](https://travis-ci.org/saep/nvim-hs.svg?branch=master)](https://travis-ci.org/saep/nvim-hs)

# What do I have to expect if I were to use it now?

* There may still be API changes, but I'm confident that they will be minor.
* Neovim might hang while waiting for an answer of a request because
  the exception handling is not very well tested because it hasn't occured yet. ;-)
* And of course, check the issue list here on github.

# How do I start using this?

All you need to know is inside the [Neovim](https://github.com/saep/nvim-hs/blob/master/library/Neovim.hs) module. 

There are also some additional examples:
* There is low [level example](https://github.com/saep/nvim-hs/blob/master/TestPlugins.hs)
  run by the tests that execute [this script](https://github.com/saep/nvim-hs/blob/master/TestPlugins.vim).
  This example does not have any convenience code generation and is
  a good example to see what the internals need for information.
* A more high [level plugin](https://github.com/saep/nvim-hs/blob/master/library/Neovim/Plugin/ConfigHelper.hs)
  is currently developed that is specifically
  designed to help in writing new plugins using this plugin provider.
* A simple configuration which tests new features and helps 
  finding issues in our API is hosted here: 
  https://github.com/saep/nvim-hs-config-example
  This should be in sync with the master branch most of the time.
  Feel free to poke me on irc or create an issue it that is not the case.

If you import the `Neovim` module into ghci, you can tab-complete the list
of generated neovim functions by typing `vim_` and hitting `<Tab>`. The functions
are meant to be self-explanatory, but if they aren't feel free to link to the documentation
on the [neovim](https://github.com/neovim/neovim) side. 

# Coordinate your work

Talk to osa1 or saep on #neovim on freenode!

I (saep) am always happy to receive pull requests that solve an issue in the tracker or that
adds good documentation. If you want to bring forwar new features or convenience libraries
for interacting with neovim, you should create an issue first. Due to the design goals of
this (cabal) project, such functionality could be added to another project (similar to how
XMonad does it with xmonad-contrib) as keeping the code base for this middleware project
small helps reducing development time as for some tasks it is necessary to issue `cabal install`
in rapid succession.

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
6. Follow the instructions at https://github.com/saep/nvim-hs-config-example/blob/master/README.md

## Normal installation

1. Make sure that the `nvim` executable is on your path
2. `cabal install /path/to/where/you/cloned/this/repository`
3. Follow the instructions at https://github.com/saep/nvim-hs-config-example/blob/master/README.md
