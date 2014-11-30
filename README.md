# nvim-hs

Neovim API for Haskell plugins as well as the plugin provider

# What has been done so far?

* The code can be generated, although the generated code may change if the need arises.

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
