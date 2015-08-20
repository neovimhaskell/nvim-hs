# nvim-hs

Neovim API for Haskell plugins as well as a plugin provider.
This library and executable should provide a basis for developing
plugins. This package should only contain broadly useful interfaces
to write plugins for neovim in haskell. The design goal is to create
an easy to use API that avoids most of the boilerplate while still retaining
some sense of reliability and type safety. Since Template Haskell is used
to generate the neovim bindings and to avoid some of the boilerplate
handy work, some exotic operating systems and architectures may not work.

[![Build Status](https://travis-ci.org/neovimhaskell/nvim-hs.svg?branch=master)](https://travis-ci.org/neovimhaskell/nvim-hs)
[![Hackage version](https://img.shields.io/hackage/v/nvim-hs.svg?style=flat)](https://hackage.haskell.org/package/nvim-hs)

# What do I have to expect if I were to use it now?

Check the issue list here on github.

# How do I start using this?

All you need to know is inside the
[Neovim](https://github.com/neovimhaskell/nvim-hs/blob/master/library/Neovim.hs)
module ([hackage](http://hackage.haskell.org/package/nvim-hs-0.0.5/docs/Neovim.html)).

# Contributing

Documentation, typo fixes and alike will almost always be merged.

If you want to bring forward new features or convenience libraries
for interacting with neovim, you should create an issue first. The features
of this (cabal) project should be kept small as this helps
reducing the development time. (For some tests it is
necessary to issue `cabal install`, so any change to to a module can
significantly increase the compilation time.)
If your idea solves a general problem, feel free to open an issue in the
library project of nvim-hs:
[nvim-hs-contrib](https://github.com/neovimhaskell/nvim-hs-contrib)

