# nvim-hs

Neovim API for Haskell plugins as well as a plugin provider.
This library and executable should provide a basis for developing
plugins. This package should only contain broadly useful interfaces
to write plugins for Neovim in haskell. The design goal is to create
an easy to use API that avoids most of the boilerplate while still retaining
some sense of reliability and type safety. Since Template Haskell is used
to generate the neovim bindings and to avoid some of the boilerplate
handy work, some exotic operating systems and architectures may not work.

[![Hackage version](https://img.shields.io/hackage/v/nvim-hs.svg?style=flat)](https://hackage.haskell.org/package/nvim-hs)
[![nvim-hs on latest Stackage LTS](http://stackage.org/package/nvim-hs/badge/lts)](http://stackage.org/lts/package/nvim-hs)
[![nvim-hs on Stackage Nightly](http://stackage.org/package/nvim-hs/badge/nightly)](http://stackage.org/nightly/package/nvim-hs)

# What do I have to expect if I were to use it now?

Check the issue list here on github.

## For Windows users

Named pipes are not supported at the momend #103. You therefore have to start
`nvim-hs` instances by connecting to STDIN and STDOUT or TCP. By default `nvim-hs`
connects to the listen socket pointed to by the `NVIM` environment variable and 
the functions in the module `Neovim.Debug` rely on that. If you want to be able to 
run these functions, start Neovim with `nvim --listen localhost:` or similar
(This example command starts neovim with a socket listening on `localhost` and 
random a random TCP port.)

# How do I start using this?

You need to install `nvim-hs.vim`, a plugin that manages starting of `nvim-hs` plugins.
To do that, just follow the instructions outlined [here](https://github.com/neovimhaskell/nvim-hs.vim).

Once you have installed `nvim-hs.vim`, you can use `nvim-hs` plugins as you would
normal vim plugins. Note that every plugin you install is started as a separate process,
which should be fine unless you have a lot of them.

# Scripting with Haskell

The entry point for all Haskell-based scripts is a plugin.
An `nvim-hs` plugin is a plain Haskell project with two conventions:

1. You need an executable that starts a `msgpack-rpc` compatible client.

2. You need a tiny bit of `vimL` in your runtime path that starts the plugin.

The simplest way to get started is using the stack template from this
repository/package inside your Neovim configuration folder, but you can also
manually create a project by doing everything that is explained in `:help nvim-hs.txt` 
(which should be available if you installed `nvim-hs.vim` as mentioned in the previous section).

To use that template, you'll first need to [install stack](https://docs.haskellstack.org/en/stable/README/)
and have the Neovim executable on the path (the API code generation calls `nvim --api-info` so it needs access to `nvim`).

After you've done that, you can run these commands to setup the template (assuming your Neovim configuration folder
is in `$XDG_CONFIG_HOME/nvim`):

```
$ cd $XDG_CONFIG_HOME/nvim
$ stack new my-nvim-hs \
https://raw.githubusercontent.com/neovimhaskell/nvim-hs/master/stack-template.hsfiles \
--bare --omit-packages --ignore-subdirs
```

If you start Neovim now, it will compile the example plugins which may take a
few minutes. Once it is started you can use the predefined functions from the
template, for example by running `:echo NextRandom()`, which should print a random number.

To start writing your own functions and plugins, read through the files
generated by the template and also check out the
[library documentation on hackage](http://hackage.haskell.org/package/nvim-hs).

# Contributing

Documentation, typo fixes, and the like will almost always be merged.

If you want to bring forward new features or convenience libraries
for interacting with Neovim, you should create an issue first. The features
of this (cabal) project should be kept small as this helps
reduce the development time. (For some tests it is
necessary to issue `cabal install`, so any change to to a module can
significantly increase the compilation time.)
If your idea solves a general problem, feel free to open an issue in the
library project of `nvim-hs`, 
[`nvim-hs-contrib`](https://github.com/neovimhaskell/nvim-hs-contrib).

