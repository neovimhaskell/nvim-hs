{- |
Module      :  Neovim
Description :  API for the neovim plugin provider /nvim-hs/
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC (due to Template Haskell)

This module should contain all the things you need to write neovim plugins in
your favorite language! @:-)@

The documentation in this module should enable you to write plugins.
The chapters in this module start with a tl;dr paragraph that sums things up,
which is useful to get an idea whether you should actually read the chapter and
which will reduce your reading time if you just want to refresh your memory.
-}
module Neovim (
    -- * Installation
    -- ** tl;dr
    -- $tldrinstallation

    -- ** Explained
    -- $explainedinstallation

    -- * Tutorial
    -- ** tl;dr
    -- $tldrtutorial
    Neovim,
    Neovim',
    neovim,
    NeovimConfig(..),
    def,

    -- ** Using existing plugins
    -- $existingplugins

    -- ** Creating a plugin
    -- $creatingplugins
    NeovimPlugin(..),
    Plugin(..),
    NvimObject(..),
    Dictionary,
    Object(..),
    wrapPlugin,
    function,
    function',
    command,
    command',
    autocmd,
    Synchronous(..),
    CommandOption(..),
    RangeSpecification(..),
    CommandArguments(..),
    AutocmdOptions(..),
    addAutocmd,
    addAutocmd',

    ask,
    asks,
    put,
    get,
    gets,
    modify,

    -- ** Creating a stateful plugin
    -- $statefulplugin

    -- ** Calling remote functions
    -- $remote
    wait,
    wait',
    waitErr,
    waitErr',
    err,
    -- ** Generated functions for neovim interaction
    module Neovim.API.String,

    -- * Unsorted exports
    -- This section contains just a bunch of more or less useful functions which
    -- were not introduced in any of the previous sections.
    liftIO,
    withCustomEnvironment,
    whenM,
    unlessM,
    Priority(..),
    module Control.Monad,
    module Control.Applicative,
    module Data.Monoid,
    module Data.Int,

    ) where

import           Control.Applicative
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Default            (def)
import           Data.Int                (Int16, Int32, Int64, Int8)
import           Data.MessagePack        (Object (..))
import           Data.Monoid
import           Neovim.API.String
import           Neovim.API.TH           (autocmd, command, command', function,
                                          function')
import           Neovim.Classes          (Dictionary, NvimObject (..))
import           Neovim.Config           (NeovimConfig (..))
import           Neovim.Context          (Neovim, Neovim', ask, asks, err, get,
                                          gets, modify, put)
import           Neovim.Main             (neovim)
import           Neovim.Plugin           (addAutocmd, addAutocmd')
import           Neovim.Plugin.Classes   (AutocmdOptions (..),
                                          CommandArguments (..), CommandOption (CmdSync, CmdRegister, CmdRange, CmdCount, CmdBang),
                                          RangeSpecification (..),
                                          Synchronous (..))
import           Neovim.Plugin.Internal  (NeovimPlugin (..), Plugin (..),
                                          wrapPlugin)
import           Neovim.RPC.FunctionCall (wait, wait', waitErr, waitErr')
import           Neovim.Util             (unlessM, whenM, withCustomEnvironment)
import           System.Log.Logger       (Priority (..))

-- Installation {{{1
-- tl;dr installation {{{2
{- $tldrinstallation

Since this is still very volatile, I recommend using a sandbox.

Make sure that neovim's executable (@nvim@) is on your @\$PATH@ during the following steps!

Install `nvim-hs` from git (example assumes you clone to @\$HOME\/git\/nvim-hs@)
using a sandbox:

@
\$ mkdir -p ~\/git ; cd ~\/git
\$ git clone https:\/\/github.com\/neovimhaskell\/nvim-hs
\$ cd nvim-hs
\$ cabal sandbox init
\$ cabal install
@

Copy the script @nvim-hs-devel.sh@ to a location you like, make it executable
and follow the brief instructions in the comments.

@
\$ cp nvim-hs-devel.sh ~\/bin\/
\$ chmod +x ~\/bin\/nvim-hs-devel.sh
@

Assuming you have copied the script to @\$HOME\/bin\/nvim-hs-devel.sh@,
put this in your neovim config file (typically @~\/.nvimrc@ or @~\/.nvim\/nvimrc@):

@
if has(\'nvim\') \" This way you can also put it in your vim config file
  function! s:RequireHaskellHost(name)
    \" If the nvim-hs script\/executable is not on your path, you should give the full path here
    return rpcstart(expand(\'$HOME\/bin\/nvim-hs-devel.sh\'), [a:name.name])
  endfunction

  \" You can replace \'haskell\' in the following lines with any name you like.
  call remote\#host\#Register(\'haskell\', \'*.[cl]\\?hs\', function(\'s:RequireHaskellHost\'))
  \" Blocks until nvim-hs has started (optional)
  call rpcrequest(remote\#host\#Require(\'haskell\'),
          \\ \'PingNvimhs\', [])
endif
@

-}
-- 2}}}

-- Explained {{{2
{- $explainedinstallation

You essentially have to follow the instructions of the tl;dr subsection above,
but this subsection tells you why you need those steps and it gives you the
required knowledge to deviate from those instructions.

If you want to use or write plugins written in haskell for /nvim-hs/, you first
have to make sure that neovim is installed and that it is available on your
@\$PATH@ during the compilation of /nvim-hs/. Neovim emits information about its
remotely callable API if you call it with the `--api-info` argument. This output
is used to generate the API functions you desperately need to create useful
plugins. Also, some internal functionality requires some of these functions.

The instructions to install /nvim-hs/ should be self-explanatory. In any case, I
(saep) recommend using a sandbox for now since there is no stable hackage
release yet and a few libraries are newer than what is currently in stackage
(namely mtl, which is a big deal). Using a sandbox requires you to install all
the libraries you want or have to use in your plugins to be installed inside the
sandbox! Some Vim plugins (e.g. ghc-mod) may show weird errors inside neovim for
your configuration file because the sandbox is not inside your configuration folder.
For /nvim-hs/ you don't need to worry about that, though, because it has a
builtin plugin which puts all compile-errors in the quickfix
list automatically after you save your configuration file, so you don't need
another plugin to detect compile time errors here. But we will discuss this
later in more detail. The executable script sets up the build environment for
/nvim-hs/ to use the sandbox.

The Vim-script snippet is a bit lengthy, but the comments should explain how it
works. In any case, the snippet can be put anywhere in your neovim configuration
file and the last call of `rpcrequest` is not needed if you don't call any
functionality before /nvim-hs/ has started properly. Removing the call can
improve the startup time. If you only need some functionality for Haskell source
files, you could move those last (or the last two) lines at the top of
@\$HOME\/.nvim\/ftplugin\/haskell.vim@. You may wonder why we have to explicitly
call 'PingNvimhs' with the function `rpcrequest` here. The short answer is:
The internals for registering functions from a remote host require this. The
longer answer is as follows: Registering functions from a remote host does not
define a function directly. It instead installs a hook via an autocmd that
defines the function. This way, only functions that are actually used are
registered and this probably was implemented this way for performance reasons.
Buf, if we try to call a function from a remote host too early, the hooks may
not yet be in place and we receive error messages. Since we do not generate any
Vim-script files which contain those hooks, /nvim-hs/ must be started and
initialized and create those hooks. So the best way to make sure that /nvim-hs/
is initialized is to try to call some functionon the msgpack-rpc channel that
/nvim-hs/ listens on. The function must not even exist, but not throwing an
error message is probably nicer, so /nvim-hs/ provides a function \"PingNvimhs\"
which takes no arguments and returns @\"Pong\"@.

Using /nvim-hs/ essentially means to use a static binary that incorporates all
plugins. It is generated using the 'Dyre' library and the binary itself is found
in @\$XDG_CACHE_DIR\/nvim@ (usually @~\/.cache\/nvim@). The 'Dyre' library makes
it feel more like a scripting language, because the binary is automatically
created and executed without having to restart neovim.

-}
-- 2}}}
-- 1}}}

-- Tutorial {{{1
-- tl;dr {{{2
{- $tldrgettingstarted
Create a file called @nvim.hs@ in @\$XDG_CONFIG_HOME\/nvim@ (usually
@~\/.config\/nvim@ with the following content:

@
\{\-\# LANGUAGE TemplateHaskell   \#\-\}
import           Neovim

main = 'neovim' 'def'
@

Adjust the fields in @def@ according to the the parameters in 'NeovimConfig'.

-}
-- 2}}}

-- Existing Plugins {{{2
{- $existingplugins
/nvim-hs/ is all about importing and creating plugins. This is done following
simple and concise API. Let's start by making a given plugin available inside
our plugin provider. Assuming that we have installed a cabal package that exports
an @examplePlugin@ from the module @TestPlugin.ExamplePlugin@. A minimal
configuration would then look like this:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}

import TestPlugin.ExamplePlugin (examplePlugin)

main = 'neovim' 'def'
        { plugins = [examplePlugin]
        }
@

That's all you have to do! Multiple plugins are simply imported and put in a
list.

If the plugin is not packaged, you can also put the source files of the plugin
inside @\$XDG_CONFIG_HOME\/nvim\/lib@ (usually @~\/.config\/nvim\/lib@).
Assuming the same module name and plugin name, you can use the same configuration
file. The source for the plugin must be located at
@\$XDG_CONFIG_HOME\/nvim\/lib\/TestPlugin\/ExamplePlugin.hs@ and all source
files it depends on must follow the same structure. This is the standard way
how Haskell modules are defined in cabal projects. Having all plugins as source
files can increase the compilation times, so plugins should be put in a cabal
project once they are mature enough. This also makes them easy to share!

-}
-- 2}}}
-- Creating a plugin {{{2
{- $creatingplugins
Creating plugins isn't difficult either. You just have to follow a simple API
and not be surprised about compile time errors of seemingly valid code. This may
sound scary, but it is not so bad. We will cover most pitfalls in the following
paragraphs and if there isn't a solution for your error, you can always ask any
friendly Haskeller in \#haskell on @irc.freenode.net@!

Enough scary stuff said for now, let's write a plugin!
Due to a stage restriction in GHC when using Template Haskell, we must define
our functions in a different module than @\$XDG_CONFIG_HOME\/nvim\/nvim.hs@.
This is a bit unfortunate, but it will save you a lot of boring boilerplate and
it will present you with helpful error messages if your plugin's functions do
not work together with neovim.

So, let's write a plugin that calculates the @n@th Fibonacci number. Don't we all
love those!

File @~\/.config\/nvim\/lib\/MyFirstPlugin.hs@

@
module MyFirstPlugin
    ( fibonacci
    ) where

import Neovim

fibonacci :: 'Int' -> Neovim' 'String'
fibonacci n = 'show' $ fibs !! n
  where
    fibs :: ['Integer']
    fibs = 0:1:'scanl1' (+) fibs
@

File @~\/.config\/nvim\/nvim.hs@:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}

import MyFirstPlugin (fibonacci)

fibonacciPlugin = 'wrapPlugin'
    { 'exports' = [ $('function'' 'fibonacci) 'Sync' ]
    }

main = 'neovim' 'def'
        { 'plugins' = [fibonacciPlugin]
        }
@

Let's analyze how it works. The module @MyFirstPlugin@ simply defines a function
that takes the @n@th element of the infinite list of Fibonacci numbers. Even though
the definition is very concise and asthetically pleasing, the important part is the
type signature for @fibonacci@. Similarly how @main :: IO ()@ works in normal Haskell
programs, 'Neovim'' is the environment we need for plugins. Internally, it stores a
few things that are needed to communicate with neovim, but that shouldn't bother you
too much. Simply remember that every plugin function must have a function signature
whose last element is of type @'Neovim' r st something@'. The result of @fibonacci@
is 'String' because neovim cannot handle big numbers so well. :-)
You can use any argument or result type as long as it is an instance of 'NvimObject'.

The second part of of the puzzle, which is the definition of @fibonacciPlugin@
in @~\/.config\/nvim\/nvim.hs@, shows what a plugin is. It is essentially two
lists of stateless and stateful functionality. A functionality can currently be one
of three things: a function, a command and an autocmd in the context of vim
terminology. In the end, all of those functionalities map to a function at the side
of /nvim-hs/. If you really want to know what the distinction between those, you
have to consult the @:help@ pages of neovim (e.g. @:help :function@, @:help :command@
and @:help :autocmd@). What's relevant from the side of /nvim-hs/ is the distinction
between __stateful__ and __stateless__. A stateless function can be called at any
time and it does not share any of its internals with other functions. A stateful
function on the other hand can share a well-defined amount of state with other
functions and in the next section I will show you a simple example for that.
Anyhow, if you take a look at the type alias for 'Neovim', you notice the two
type variables @r@ and @st@. These can be accessed with different semantics
each. A value of type @r@ can only be read. It is more or less a static value
you can query with 'ask' or 'asks' if you
are inside a 'Neovim' environment. The value @st@ can be changed and those
changes will be available to other functions which run in the same environment.
You can get the current value with 'get', you can replace an existing value with
'put' and you can also apply a function to the current state with 'modify'.
Notice how 'Neovim'' is just a specialization of 'Neovim' with its @r@ and
@st@ set to @()@.

Now to the magical part: @\$('function'' 'fibonacci)@. This is a so called
Template Haskell splice and this is why you need
@\{\-\# LANGUAGE TemplateHaskell \#\-\}@ at the top of your Haskell file. This
splice simply generates Haskell code that, in this case, still needs a value
of type 'Synchronous' which indicates whether calling the function will make
neovim wait for its result or not. Internally, the expression
@\$('function'' 'fibonacci) 'Sync'@ creates a value that contains all the
necessary information to properly register the function with neovim. Note the
prime symbol before the function name! This would have probably caused you
some trouble if I haven't mentioned it here! Template Haskell simply requires
you to put that in front of function names that are passed in a splice.

If you compile this (which should happen automatically if you have put those
files at the appropriate places), you can calculate the 287323rd Fibonacci number
like this:

@
:echo Fibonacci(287323)
@

You can also directly insert the result inside any text file opened with neovim
by using the evaluation register by pressing the following key sequence in insert
mode:

@
\<C-r\>=Fibonacci(287323)
@

-}
-- 2}}}
-- Creating a stateful plugin {{{2
{- $remote
Now that we are a little bit comfortable with the interface provided by /nvim-hs/,
we can start to write a more complicated plugin. Let's create a random number
generator.

File @~\/.config\/nvim\/lib\/MyRandomNumberGenerator.hs@:

@
module MyRandomNumberGenerator
    ( nextRand
    , setNextNumber
    ) where

nextRand :: 'Neovim' r ['Int16'] Int16
nextRand = do
    r <- gets head
    modify tail
    return r

setNextNumber :: Int16 -> 'Neovim' r ['Int16'] ()
setNextNumber n = modify (n:)
@

File @~\/.config\/nvim\/nvim.hs@:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}

import MyRandomNumberGenerator (nextRand, setNextNumber)
import System.Random (newStdGen, randoms)

randPlugin = do
    g <- newStdGen                -- initialize with a random seed
    let randomNumbers = randoms g -- an infite list of random numbers
    'wrapPlugin'
        { 'statefulExports' =
            [ ((), randomNumbers,
                [ $('function'' 'nextRand) 'Sync'
                , $('function'' 'setNextNumber) 'Async'
                ])
            ]
        }

main = 'neovim' 'def'
        { 'plugins' = [randPlugin]
        }
@

That wasn't too hard, was it? The definition is very similar to the previous
example, we just were able to mutate our state and share that with other
functions. The only slightly tedious thing was to define the 'statefulExpors'
field because it is a list of triples which has a list of exported functionality
as its third argument.

-}
-- 2}}}
-- Calling remote functions {{{2
{- $remote
Calling remote functions is only possible inside a 'Neovim' context. There are
a few patterns of return values for the available functions. Let's start with
getting some abstract 'Buffer' object, test whether it is valid and then try
to rename it.

@
inspectBuffer :: 'Neovim' r st ()
inspectBuffer = do
    cb <- 'vim_get_current_buffer'
    isValid <- 'buffer_is_valid' cb
    when isValid $ do
        let newName = "magic"
        retval <- 'wait'' $ 'buffer_set_name' cb newName
        case retval of
            Right cbName | cbName == newName -> 'return' ()
            Right _ -> 'err' $ "Renaming the current buffer failed!"
            Left e -> 'err' $ 'show' e
@

You may have noticed the 'wait'' function in there. Some functions have a return
type with 'STM' in it. This means that the function call is asynchronous. We can
'wait' (or 'wait'') for the result at the point at which we actually need it. In
this short example, we put the 'wait'' directly in front of the remote function
call because we want to inspect the result immediately, though. The other
functions either returned a result directly or they returned
@'Either' 'Object' something@ whose result we inspected ourselves. The 'err'
function directly terminates the current thread and sends the given error
message to neovim which the user immediately notices. Since it is not unusual to
not know what to do if the remote function call failed, the functions 'waitErr'
and 'waitErr'' can save you from some typing and deeply nested case expressions.

That's pretty much all there is to it.
-}
-- 2}}}
-- 1}}}

-- vim: foldmethod=marker
