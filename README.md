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
[![nvim-hs on latest Stackage LTS](http://stackage.org/package/nvim-hs/badge/lts)](http://stackage.org/lts/package/nvim-hs)
[![nvim-hs on Stackage Nightly](http://stackage.org/package/nvim-hs/badge/nightly)](http://stackage.org/nightly/package/nvim-hs)

# What do I have to expect if I were to use it now?

Check the issue list here on github.

# How do I start using this?

First, you have to decide how you want to manage your plugins. Every section
of this chapter describes an alternative way to manage your plugins. It
starts with a list of pros and cons and then explains what you have to do to
get rolling and how you would install a `nvim-hs`-compatible plguin. The
general library documentation is in the haddocks on
([hackage](http://hackage.haskell.org/package/nvim-hs-0.2.0/docs/Neovim.html)).
If you are new to Haskell development or you don't really care how you manage
your plugins, I (saep) recommend the stack template approach.

## Stack via the template

### Pros

- Easy to setup because of the template
- Flexible dependency management; everything that stack supports can be done, this
  includes packages on stackage, packages on hackage, local packages and repositories
- Reprobucible; if it works once, it will work in the future
- If you don't use stack just for neovim plugins and you have other projects with the
  same (or a similiar) lts (long term support) version, you save compilation time on the
  initial setup

### Cons

- A bit verbose; you have to add dependencies twice if they are not in the stackage snapshot

### Installation

First, you must [install stack](https://docs.haskellstack.org/en/stable/README/).

You have to have installed neovim and the executable `nvim` must be on the path.
(The API code generation calls `nvim --api-info`.)

Afterwards, you switch to your neovim configuration folder (typically `~/.config/nvim`) and
you have to create your plugin project.

> cd ~/.config/nvim

> stack new my-nvim-hs https://raw.githubusercontent.com/neovimhaskell/nvim-hs/master/stack-template.hsfiles --bare --omit-packages --ignore-subdirs

Now, you have to compile everything.

> stack setup

> stack build

If there are no errors (there shouldn't be any), you only have to tell neovim how to start this.
Add the following to your `init.vim`:

```vimL
if has('nvim') " This way, you can also put this in your plain vim config

	" function which starts a nvim-hs instance with the supplied name
	function! s:RequireHaskellHost(name)
		" It is important that the current working directory (cwd) is where
		" your configuration files are.
		return jobstart(['stack', 'exec', 'nvim-hs', a:name.name], {'rpc': v:true, 'cwd': expand('$HOME') . '/.config/nvim'})
	endfunction

	" Register a plugin host that is started when a haskell file is opened
	call remote#host#Register('haskell', "*.l\?hs", function('s:RequireHaskellHost'))

	" But if you need it for other files as well, you may just start it
	" forcefully by requiring it
	let hc=remote#host#Require('haskell')
endif
```

If you start neovim now, you can use the predefined functions from the template.

> :echo NextRandom()

should print a random number.

### Installing a plugin from Hackage

Let's take [nvim-hs-ghcid](http://hackage.haskell.org/package/nvim-hs-ghcid)
as an example. Let's also pretend, that it's not on stackage. 
We have to declare the dependency
in the `my-nvim-hs.cabal` file and in the `stack.yaml` file. In the `.cabal`
file, add `nvim-hs-ghcid` to the `build-depends` section. It should look
like this:

```cabal
  build-depends:       base >= 4.7 && < 5
                     , nvim-hs >= 0.2.0 && < 1.0.0
                     , nvim-hs-ghcid
```

You can omit the version number, since you will have to define it in the
`stack.yaml` file and you are managing you dependencies with stack anyways.
The `extra-deps` section of the `stack.yaml` should look like this:

```yaml
extra-deps:
- nvim-hs-ghcid-0.2.0
```

If `nvim-hs-ghcid` depended upon any other package that is not on stackage,
you would have to add those dependencies there as well. The output of 
`stack build` should tell you which you have to add. You don't have to add
these transitive dependencies to the `build-depends` of the cabal file because
you are not accessing anything from these packages directly.

Adding all these explicit versions seems to be the disadvantage of using stack. 
However, the benefit is that you will have a
reproducible build in the future and you don't have to hunt down a working
set of version boundaries for every dependency you have. A little effort now
will save you more time later!

To use the plugin, add it to the plugins list of the `nvim.hs` file in
`~/.config/nvim`:

```haskell
import Neovim

import qualified Neovim.Example.Plugin as Example
import qualified Neovim.Ghcid as Ghcid

main :: IO ()
main = do
    neovim defaultConfig
        { plugins = plugins defaultConfig ++
            [ Example.plugin
            , Ghcid.plugin
            ]
        }
```

If you want to update a dependency/plugin,
you have to manually increment the version number in the stack.yaml file and
possibly fix the compilation errors that arise. If you want a rolling
release for a plugin, follow the instructions for installing a plugin from
git.

### Installing a plugin from git

This method is best suited for plugins that update a lot and for which you need
the most recent version most of the time. This also works for plugins that do not
have a hackage release. If you don't intend to work on the
code of that plugin repository, you can add it to the plugin list of your
plugin manager (e.g. [vim-plug](https://github.com/junegunn/vim-plug)). 
This way, you get updates if you update all your normal vim plugins.
To stay with the example of the previous section, we use the `nvim-hs-ghcid`
plugin again.

Add the plugin to your plugin manager (here with vim-plug as an example):

```vimL
Plug 'saep/nvim-hs-ghcid', { 'for': ['haskell'] }
```

Once vim-plug has cloned or updated the repository, add the plugin to the
packages list of the `stackage.yaml` file. The packages list should look
like this:

```yaml
packages:
- .
- plugged/nvim-hs-ghcid # or the appropriate relative path the folder you configured
```

As long as you have the repository in this list, you don't have to specify
it as a dependency  anywhere else, you still have to add the plugins'
dependencies to the `stack.yaml` file, though. It chould look like this:

```yaml
extra-deps:
- some-dependency-0.2.4
- and-another-one-13.8.5.2
```

Add the plugin to the plugins list in `nvim.hs` in exactly the same way as
described at the end of the previous chapter.

The downside of this approach is that your compilation times will be longer the more
plugins you include this way. 

### Writing your own functions that you can call from neovim

The stack template generated a few files for you that you can use as a
template to write your own plugins. If you edit them and make a mistake that
the Haskell compiler can detect, an item in the quickfix list should appear.
This is, unless you removed `plugins defaultConfig` from `nvim.hs`.

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

