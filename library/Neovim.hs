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

The documentation in this module should provide every information you need to start
writing plugins.
-}
module Neovim (
    -- * Installation
    -- $installation

    -- * Tutorial
    -- ** Motivation
    -- $overview
    -- ** Combining existing plugins
    -- $existingplugins
    Neovim,
    neovim,
    NeovimConfig(..),
    defaultConfig,
    def,


    -- ** Creating a plugin
    -- $creatingplugins
    NeovimPlugin(..),
    Plugin(..),
    NvimObject(..),
    (+:),
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

    ask,
    asks,

    -- ** Creating a stateful plugin
    -- $statefulplugin

    -- ** Calling remote functions
    -- $remote
    wait,
    wait',
    err,
    errOnInvalidResult,
    NeovimException(..),

    -- * Unsorted exports
    -- This section contains just a bunch of more or less useful functions which
    -- were not introduced in any of the previous sections.
    liftIO,
    whenM,
    unlessM,
    docToObject,
    docFromObject,
    Doc,
    AnsiStyle,
    Pretty(..),
    putDoc,
    exceptionToDoc,
    Priority(..),
    module Control.Monad,
    module Control.Applicative,
    module Data.Monoid,
    module Data.Int,
    module Data.Word,

    ) where

import           Control.Applicative
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Default                 (def)
import           Data.Int                     (Int16, Int32, Int64, Int8)
import           Data.MessagePack             (Object (..))
import           Data.Monoid
import           Data.Word                    (Word, Word16, Word32, Word8)
import           Neovim.API.TH                (autocmd, command, command',
                                               function, function')
import           Neovim.Classes               (Dictionary, NvimObject (..),
                                               Doc, AnsiStyle, Pretty(..),
                                               docFromObject, docToObject, (+:))
import           Neovim.Config                (NeovimConfig (..))
import           Neovim.Context               (Neovim,
                                               NeovimException(..),
                                               exceptionToDoc,
                                               ask, asks, err,
                                               errOnInvalidResult)
import           Neovim.Main                  (neovim)
import           Neovim.Plugin                (addAutocmd)
import           Neovim.Plugin.Classes        (AutocmdOptions (..),
                                               CommandArguments (..),
                                               CommandOption (CmdBang, CmdCount, CmdRange, CmdRegister, CmdSync),
                                               RangeSpecification (..),
                                               Synchronous (..))
import           Neovim.Plugin.Internal       (NeovimPlugin (..), Plugin (..),
                                               wrapPlugin)
import           Neovim.RPC.FunctionCall      (wait, wait')
import           Neovim.Util                  (unlessM, whenM)
import           System.Log.Logger            (Priority (..))
import           Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
-- Installation {{{1
{- $installation

Installation instructions are in the README.md file that comes with the source
of this package. It is also on the repositories front page.

-}
-- 1}}}

-- Tutorial {{{1
-- Overview {{{2
{- $overview
An @nvim-hs@ plugin is just a collection of haskell functions that can be
called from neovim.

As a user of plugins, you basically have two choices. You can start every
plugin in a separate process and use normal vim plugin management strategies
such as <https://github.com/junegunn/vim-plug vim-plug>
or <https://github.com/tpope/vim-pathogen pathogen>.
Alternatively, you can create a haskell project and depend on the plugins
you want to use and plumb them together. This plumbing is equivalent to
writing a plugin.

Since you are reading haddock documentation, you probably want the latter, so
just keep reading. @:-)@

-}


-- 2}}}
-- Combining Existing Plugins {{{2
{- $existingplugins
The easiest way to start is to use the stack template as described in the
@README.md@ of this package. If you initialize it in your neovim configuration
directory (@~\/.config\/nvim@ on linux-based systems), it should automatically be
compiled and run with two simple example plugins.

You have to define a haskell project that depends on this package and
contains an executable secion within a main file that looks something like this:

@
import TestPlugin.ExamplePlugin (examplePlugin)

main = 'neovim' 'def'
        { 'plugins' = [ examplePlugin ] ++ 'plugins' 'defaultConfig'
        }
@


/nvim-hs/ is all about importing and creating plugins. This is done following a
concise API. Let's start by making a given plugin available inside
our plugin provider. Assuming that we have installed a cabal package that exports
an @examplePlugin@ from the module @TestPlugin.ExamplePlugin@, a minimal
main file would look something like this:

That's all you have to do! Multiple plugins are simply imported and put in a
list.

-}

-- | Default configuration options for /nvim-hs/. If you want to keep the
-- default plugins enabled, you can define your config like this:
--
-- @
-- main = 'neovim' 'defaultConfig'
--          { plugins = plugins defaultConfig ++ myPlugins
--          }
-- @
--
defaultConfig :: NeovimConfig
defaultConfig = Config
    { plugins      = []
    , logOptions   = Nothing
    }


-- 2}}}
-- Creating a plugin {{{2
{- $creatingplugins
Creating plugins isn't difficult either. You just have to follow and survive the
compile time errors of seemingly valid code. This may sound scary, but it is not
so bad. We will cover most pitfalls in the following paragraphs and if there
isn't a solution for your error, you can always ask any friendly Haskeller in
\#haskell on @irc.freenode.net@!

Enough scary stuff said for now, let's write a plugin!
Due to a stage restriction in GHC when using Template Haskell
(i.e. code generation), we must define
our functions in a different module than @\$XDG_CONFIG_HOME\/nvim\/nvim.hs@.
(I'm assuming here, that you use @\$XDG_CONFIG_HOME\/nvim\/@ as the base
directory for historical reasons and because it might be an appropriate place.)
This is a bit unfortunate, but it will save you a lot of boring boilerplate and
it will present you with helpful error messages if your plugin's functions do
not work together with neovim.

So, let\'s write a plugin that calculates the @n@th Fibonacci number. Don\'t we all
love those!

File @\~\/.config\/nvim\/lib\/Fibonacci\/Plugin.hs@:

@
module Fibonacci.Plugin (fibonacci) where

import "Neovim"

\-\- \| Neovim is not really good with big numbers, so we return a 'String' here.
fibonacci :: 'Int' -> 'Neovim' env 'String'
fibonacci n = 'return' . 'show' \$ fibs !! n
  where
    fibs :: [Integer]
    fibs = 0:1:'scanl1' (+) fibs
@

File @\~\/.config\/nvim\/lib\/Fibonacci.hs@:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}
module Fibonacci (plugin) where

import "Neovim"
import Fibonacci.Plugin (fibonacci)

plugin :: 'Neovim' () 'NeovimPlugin'
plugin = 'wrapPlugin' Plugin
    { 'environment' = ()
    , 'exports'     = [ $('function'' 'fibonacci) 'Sync' ]
    }
@

File @~\/.config\/nvim\/nvim.hs@:

@
import "Neovim"

import qualified Fibonacci as Fibonacci

main :: 'IO' ()
main = 'neovim' 'defaultConfig'
    { 'plugins' = 'plugins' 'defaultConfig' ++ [ Fibonacci.plugin ]
    }
@

Let's analyze how it works. The module @Fibonacci.Plugin@ simply defines a function
that takes the @n@th element of the infinite list of Fibonacci numbers. Even though
the definition is very concise and asthetically pleasing, the important part is the
type signature for @fibonacci@. Similarly how @main :: IO ()@ works in normal Haskell
programs, 'Neovim' is the environment we need for plugins. Internally, it stores a
few things that are needed to communicate with neovim, but that shouldn't bother you
too much. Simply remember that every plugin function must have a function signature
whose last element is of type @'Neovim' env something@. The result of @fibonacci@
is 'String' because neovim cannot handle big numbers so well. :-)
You can use any argument or result type as long as it is an instance of 'NvimObject'.

The second part of of the puzzle, which is the definition of @plugin@
in @~\/.config\/nvim\/lib\/Fibonacci.hs@, shows what a plugin is. It is essentially
an empty environment and a list of functions, commands or autocommands in the context of vim
terminology.  In the end, all of those things map to a function at the side
of /nvim-hs/. If you really want to know what the distinction between those is, you
have to consult the @:help@ pages of neovim (e.g. @:help :function@, @:help :command@
and @:help :autocmd@). What's relevant from the side of /nvim-hs/ is the environment.
The environment is a data type that is avaiable to all exported functions of your
plugin. This example does not make use of anything of that environment, so
we used '()', also known as unit, as our environment. The definition of
@fibonacci@ uses a type variable @env@ as it does not access the environment and
can handle any environment. If you want to access the environment, you can call
'ask' or 'asks' if you are inside a 'Neovim' environment. An example that shows
you how to use it can be found in a later chapter.

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

If you compile this and restart the plugin, you can calculate the 2000th Fibonacci
number like as if it were a normal vim-script function:

@
:echo Fibonacci(2000)
@

You can also directly insert the result inside any text file opened with neovim
by using the evaluation register by pressing the following key sequence in insert
mode:

@
\<C-r\>=Fibonacci(2000)
@

The haddock documentation will now list all the things we have used up until now.
Afterwards, there is a plugin with state which uses the environment.

-}
-- 2}}}
-- Creating a stateful plugin {{{2
{- $statefulplugin
Now that we are a little bit comfortable with the interface provided by /nvim-hs/,
we can start to write a more complicated plugin. Let's create a random number
generator!

File @~\/.config\/nvim\/lib\/Random\/Plugin.hs@:

@
module Random.Plugin (nextRandom, setNextRandom) where

import "Neovim"

import System.Random (newStdGen, randoms)
import UnliftIO.STM  (TVar, atomically, readTVar, modifyTVar, newTVarIO)

-- You may want to define a type alias for your plugin, so that if you change
-- your environment, you don't have to change all type signatures.
--
-- If I were to write a real plugin, I would probably also create a data type
-- instead of directly using a TVar here.
--
type MyNeovim a = Neovim ('TVar' ['Int16']) a

-- This function will create an initial environment for our random number
-- generator. Note that the return type is the type of our environment.
randomNumbers :: Neovim startupEnv (TVar [Int16])
randomNumbers = do
    g <- liftIO newStdGen -- Create a new seed for a pseudo random number generator
    newTVarIO (randoms g) -- Put an infinite list of random numbers into a TVar

-- | Get the next random number and update the state of the list.
nextRandom :: MyNeovim 'Int16'
nextRandom = do
    tVarWithRandomNumbers <- 'ask'
    atomically $ do
        -- pick the head of our list of random numbers
        r \<- 'head' <$> 'readTVar' tVarWithRandomNumbers

        -- Since we do not want to return the same number all over the place
        -- remove the head of our list of random numbers
        modifyTVar tVarWithRandomNumbers 'tail'

        'return' r


-- | You probably don't want this in a random number generator, but this shows
-- hoy you can edit the state of a stateful plugin.
setNextRandom :: 'Int16' -> MyNeovim ()
setNextRandom n = do
    tVarWithRandomNumbers <- 'ask'

    -- cons n to the front of the infinite list
    atomically $ modifyTVar tVarWithRandomNumbers (n:)
@

File @~\/.config\/nvim\/lib\/Random.hs@:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}
module Random (plugin) where

import "Neovim"
import Random.Plugin (nextRandom, setNextRandom)
import "System.Random" ('newStdGen', 'randoms')

plugin :: 'Neovim' ('StartupConfig' 'NeovimConfig') () 'NeovimPlugin'
plugin = do
    env <- randomNumbers
    'wrapPlugin' 'Plugin'
        { environment = env
        , 'exports'         =
          [ $('function'' 'nextRandom) 'Sync'
          , $('function' \"SetNextRandom\" 'setNextRandom) 'Async'
          ]
        }
@

File @~\/.config\/nvim\/nvim.hs@:

@
import "Neovim"

import qualified Fibonacci as Fibonacci
import qualified Random    as Random

main :: 'IO' ()
main = 'neovim' 'defaultConfig'
    { 'plugins' = 'plugins' 'defaultConfig' ++ [ Fibonacci.plugin, Random.plugin ]
    }
@


That wasn't too hard, was it? The definition is very similar to the previous
example, we just were able to mutate our state and share that with other
functions. Another noteworthy detail, in case you
are not familiar with it, is the use of 'liftIO' in front of 'newStdGen'. You
have to do this, because 'newStdGen' has type @'IO' 'StdGen'@ but the actions
inside the startup code are of type
@'Neovim' () something@. 'liftIO' lifts an
'IO' function so that it can be run inside the 'Neovim' context (or more
generally, any monad that implements the 'MonadIO' type class).

After you have saved these files (and removed any typos @:-)@), you can restart
/nvim-hs/ with @:RestartNvimhs@ and insert random numbers in your text files!

@
\<C-r\>=NextRandom()
@

You can also cheat and pretend you know the next number:

@
:call SetNextRandom(42)
@

-}
-- 2}}}
-- Calling remote functions {{{2
{- $remote
Calling remote functions is only possible inside a 'Neovim' context. There are
a few patterns of return values for the available functions. Let's start with
getting some abstract 'Buffer' object, test whether it is valid and then try
to rename it.

@
inspectBuffer :: 'Neovim' env ()
inspectBuffer = do
    cb <- 'vim_get_current_buffer'
    isValid <- 'buffer_is_valid' cb
    when isValid $ do
        let newName = "magic"
        cbName \<- 'wait'' $ 'buffer_set_name' cb newName
        case () of
            _ | cbName == newName -> 'return' ()
            _ -> 'err' $ "Renaming the current buffer failed!"
@

You may have noticed the 'wait'' function in there. Some functions have a return
type with 'STM' in it. This means that the function call is asynchronous. We can
'wait' (or 'wait'') for the result at the point at which we actually need it. In
this short example, we put the 'wait'' directly in front of the remote function
call because we want to inspect the result immediately, though. The other
functions either returned a result directly or they returned
@'Either' 'Object' something@ whose result we inspected ourselves. The 'err'
function directly terminates the current thread and sends the given error
message to neovim which the user immediately notices.

That's pretty much all there is to it.
-}
-- 2}}}
-- 1}}}

-- vim: foldmethod=marker
