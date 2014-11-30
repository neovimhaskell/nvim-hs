# nvim-hs

Neovim API for Haskell plugins as well as the plugin provider

# What has been done so far?

* The code can be generated, although the generated code may change if the need arises.

# Coordinate your work

Currently I am still all alone on this project. If you want to work on something or if you have any questions, contact me. You can create a pull request/issue with what you want to do and I will (usually) comment within a day. I'm also on IRC (e.g. #neovim, #haskell on freenode) with the nick _saep_. When I'm actually online (i.e. I am connected to my bouncer), I should respond within 5 minutes if you highlight me or write a personal message.

# Installation 

## Sandbox (Recommended as dependencies are in still in flux)

1. Make sure that the `nvim` executable is on your path
2. Make sure the submodule is initialised and updated

   ```
   git submodule init
   git submodule update
   ```

3. Add the submodule to the sandbox

  ```
  cabal sandbox add-source messagepack
  ```
  
4. Type `cabal install` in the project directory
