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
    -- ** tl;dr
    -- $tldrinstallation

    -- ** Explained
    -- $explainedinstallation

    -- * Tutorial
    -- ** tl;dr
    -- $tldrgettingstarted
    Neovim,
    Neovim',
    neovim,
    NeovimConfig(..),
    defaultConfig,
    StartupConfig(..),
    def,

    -- ** Using existing plugins
    -- $existingplugins

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
    Doc,
    Pretty(..),
    errOnInvalidResult,
    text,
    NeovimException(..),
    -- ** Generated functions for neovim interaction
    module Neovim.API.String,

    -- * Unsorted exports
    -- This section contains just a bunch of more or less useful functions which
    -- were not introduced in any of the previous sections.
    liftIO,
    withCustomEnvironment,
    whenM,
    unlessM,
    docToObject,
    docFromObject,
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
import           Neovim.API.String
import           Neovim.API.TH                (autocmd, command, command',
                                               function, function')
import           Neovim.Classes               (Dictionary, NvimObject (..),
                                               docFromObject, docToObject, (+:))
import           Neovim.Config                (NeovimConfig (..))
import           Neovim.Context               (Neovim, Neovim',
                                               NeovimException (ErrorMessage),
                                               ask, asks, err,
                                               errOnInvalidResult, get, gets,
                                               modify, put)
import           Neovim.Main                  (neovim)
import           Neovim.Plugin                (addAutocmd, addAutocmd')
import           Neovim.Plugin.Classes        (AutocmdOptions (..),
                                               CommandArguments (..),
                                               CommandOption (CmdBang, CmdCount, CmdRange, CmdRegister, CmdSync),
                                               RangeSpecification (..),
                                               Synchronous (..))
import qualified Neovim.Plugin.ConfigHelper   as ConfigHelper
import           Neovim.Plugin.Internal       (NeovimPlugin (..), Plugin (..),
                                               wrapPlugin)
import           Neovim.Plugin.Startup        (StartupConfig (..))
import           Neovim.RPC.FunctionCall      (wait, wait', waitErr, waitErr')
import           Neovim.Util                  (unlessM, whenM,
                                               withCustomEnvironment)
import           System.Log.Logger            (Priority (..))
import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..), text)

-- Installation {{{1
-- tl;dr installation {{{2
{- $tldrinstallation

Make sure that neovim's executable (@nvim@) is on your @\$PATH@ during the
cabal commands!

/nvim-hs/ is a normal haskell program and a normal haskell library. You can install it
in various flavors. These steps describe a more laborous approach that is suited for
developing plugins or /nvim-hs/ itself.

The following steps will install `nvim-hs` from git
(example assumes you clone to @\$HOME\/git\/nvim-hs@)
using a sandbox:

@
\$ mkdir -p ~\/git ; cd ~\/git
\$ git clone https:\/\/github.com\/neovimhaskell\/nvim-hs
\$ cd nvim-hs
\$ cabal sandbox init
\$ cabal install
@

Or in one line for copy-pasting:

@
mkdir -p ~\/git ; cd ~\/git ; git clone https:\/\/github.com\/neovimhaskell\/nvim-hs && cd nvim-hs && cabal sandbox init && cabal install
@

Copy the script @nvim-hs-devel.sh@ to a location you like, make it executable
and __follow the brief instructions__ in the comments.

@
\$ cp nvim-hs-devel.sh ~\/bin\/
\$ chmod +x ~\/bin\/nvim-hs-devel.sh
@

Assuming you have copied the script to @\$HOME\/bin\/nvim-hs-devel.sh@,
put this in your neovim config file (typically @~\/.nvimrc@ or @~\/.nvim\/nvimrc@):

@
if has(\'nvim\') \" This way you can also put it in your vim config file
    call rpcrequest(rpcstart(expand(\'\$HOME\/.bin\/nvim-hs-devel.sh\')), \"PingNvimhs\")
endif
@

-}
-- 2}}}

-- Explained {{{2
{- $explainedinstallation

If you want to use or write plugins written in haskell for /nvim-hs/, you first
have to make sure that neovim is installed and that it is available on your
@\$PATH@ during the compilation of /nvim-hs/. Neovim emits information about its
remotely callable API if you call it with the `--api-info` command line
argument. This output is used to generate the API functions you need
to create useful plugins. Also, some internal functionality requires some of
these functions.

The instructions to install /nvim-hs/ should be self-explanatory. In any case, I
(saep) recommend using a sandbox for now since I the version constraints of the
dependencies are quire lax and there are still changes on the way. Also, there is
no official neovim release yet, so you may have to reinstall /nvim-hs/ a few times
because the generated API could change or something similar. A sandboxed environment
can be saefly deleted and it requires you only to copy and edit a small shell script!

Using a sandbox requires you to install all
the libraries you want or have to use in your plugins to be installed inside the
sandbox! Some Vim plugins (e.g. ghc-mod) may show weird errors inside neovim for
your configuration file because the sandbox is not inside your configuration folder.
For /nvim-hs/ you don't need to worry about that, though, because it has a
builtin plugin which puts all compile-errors in the quickfix
list automatically after you save your configuration file, so you don't need
another plugin to detect compile time errors here. But we will discuss this
later in more detail. The executable script mentioned in the tl;dr installation
instructions sets up the build environment for /nvim-hs/ to use the sandbox.

The Vim-script snippet is a bit conservative and may have a negative impact on
your startup time. You can remove the @rpcrequest()@ wrapping and call the function
@PingNvimhs@ at a later time when you need /nvim-hs/ to be initialized. Use your
own judgement!  In any case, the snippet can be put anywhere in your neovim
configuration. You may wonder why we have to explicitly
call @PingNvimhs@ with the function @rpcrequest@ here. The short answer is:
The internals for registering functions from a remote host require this. The
longer answer is as follows: Registering functions from a remote host does not
define a function directly. It instead installs a hook via an autocmd that
defines the function. This way, only functions that are actually used are
registered and this probably was implemented this way for performance reasons.
Buf, if we try to call a function from a remote host too early, the hooks may
not yet be in place and we receive error messages. Since we do not generate any
Vim-script files which contain those hooks, /nvim-hs/ must be started and
initialized and create those hooks. So the best way to make sure that /nvim-hs/
is initialized is to try to call some functionon on the msgpack-rpc channel that
/nvim-hs/ listens on. The function must not even exist, but not throwing an
error message is probably nicer, so /nvim-hs/ provides a function \"PingNvimhs\"
which takes no arguments and returns @\"Pong\"@.

Using /nvim-hs/ essentially means to use a static binary that incorporates all
plugins. It is generated using the 'Dyre' library and the binary itself is found
in @\$XDG_CACHE_DIR\/nvim@ (usually @~\/.cache\/nvim@). The 'Dyre' library makes
it feel more like a scripting language, because the binary is automatically
created and executed without having to restart neovim. You can also use the
functions from the "Neovim.Debug" module if you want to develop your plugins in
a REPL environment. This is probably a bit more difficult to use, so I won't go
into detail here.

-}
-- 2}}}
-- 1}}}

-- Tutorial {{{1
-- tl;dr {{{2
{- $tldrgettingstarted
If you are proficient with Haskell, it may be sufficient to point you at some of the
important data structures and functions. So, I will do it here. If you need more
assistance, please skip to the next section and follow the links for functions or data
types you do no understand how to use. If you think that the documentation is lacking,
please create an issue on github (or even better, a pull request with a fix @;-)@).
The code sections that describe new functionality are followed by the source code
documentation of the used functions (and possibly a few more).

The config directory location adheres to the
<http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG-basedir specification>.
Unless you have changed some \$XDG_\* environment variables, the configuration
directory on unixoid systems (e.g. MacOS X, most GNU/Linux distribution, most
BSD distributions) is @\$HOME\/.config\/nvim@.

Create a file called @nvim.hs@ in @\$XDG_CONFIG_HOME\/nvim@ (usually
@~\/.config\/nvim@) with the following content:

@
import Neovim

main = 'neovim' 'defaultConfig'
@

Adjust the fields in 'defaultConfig' according to the parameters in 'NeovimConfig'.
Depending on how you define the parameters, you may have to add some language extensions
which GHC should point you to.

-}


-- | Default configuration options for /nvim-hs/. If you want to keep the
-- default plugins enabled, you can define your config like this:
--
-- @
-- main = 'neovim' 'defaultConfig'
--          { plugins = myPlugins ++ plugins defaultConfig
--          }
-- @
--
defaultConfig :: NeovimConfig
defaultConfig = Config
    { plugins      = [ ConfigHelper.plugin ]
    , logOptions   = Nothing
    , errorMessage = Nothing
    }


-- 2}}}

-- Existing Plugins {{{2
{- $existingplugins
/nvim-hs/ is all about importing and creating plugins. This is done following a
concise API. Let's start by making a given plugin available inside
our plugin provider. Assuming that we have installed a cabal package that exports
an @examplePlugin@ from the module @TestPlugin.ExamplePlugin@. A minimal
configuration would then look like this:

@
import TestPlugin.ExamplePlugin (examplePlugin)

main = 'neovim' 'def'
        { 'plugins' = [ examplePlugin ] ++ 'plugins' 'defaultConfig'
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
Creating plugins isn't difficult either. You just have to follow and survive the
compile time errors of seemingly valid code. This may sound scary, but it is not
so bad. We will cover most pitfalls in the following paragraphs and if there
isn't a solution for your error, you can always ask any friendly Haskeller in
\#haskell on @irc.freenode.net@!

Enough scary stuff said for now, let's write a plugin!
Due to a stage restriction in GHC when using Template Haskell, we must define
our functions in a different module than @\$XDG_CONFIG_HOME\/nvim\/nvim.hs@.
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
fibonacci :: 'Int' -> 'Neovim'' 'String'
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

plugin :: 'Neovim' ('StartupConfig' 'NeovimConfig') () 'NeovimPlugin'
plugin = 'wrapPlugin' Plugin
    { 'exports'         = [ $('function'' 'fibonacci) 'Sync' ]
    , 'statefulExports' = []
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
programs, 'Neovim'' is the environment we need for plugins. Internally, it stores a
few things that are needed to communicate with neovim, but that shouldn't bother you
too much. Simply remember that every plugin function must have a function signature
whose last element is of type @'Neovim' r st something@. The result of @fibonacci@
is 'String' because neovim cannot handle big numbers so well. :-)
You can use any argument or result type as long as it is an instance of 'NvimObject'.

The second part of of the puzzle, which is the definition of @plugin@
in @~\/.config\/nvim\/lib\/Fibonacci.hs@, shows what a plugin is. It is essentially two
lists of stateless and stateful functionality. A functionality can currently be one
of three things: a function, a command and an autocmd in the context of vim
terminology. In the end, all of those functionalities map to a function at the side
of /nvim-hs/. If you really want to know what the distinction between those is, you
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
files at the appropriate places), you can restart /nvim-hs/ with the command
@:RestartNvimhs@ which is available as long as you do not remove the default
plugins from you rconfig. Afterwards, you can calculate the 2000th Fibonacci
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

\-\- | Neovim isn't so good with big numbers here either.
nextRandom :: 'Neovim' r ['Int16'] 'Int16'
nextRandom = do
    r <- 'gets' 'head' -- get the head of the infinite random number list
    'modify' 'tail'    -- set the list to its tail
    'return' r

setNextRandom :: 'Int16' -> 'Neovim' r ['Int16'] ()
setNextRandom n = 'modify' (n:) -- cons to the front of the infinite list
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
    g <- 'liftIO' 'newStdGen'         -- initialize with a random seed
    let randomNumbers = 'randoms' g -- an infinite list of random numbers
    'wrapPlugin' 'Plugin'
        { 'exports'         = []
        , 'statefulExports' =
            [ ((), randomNumbers,
                [ $('function'' 'nextRandom) 'Sync'
                , $('function' \"SetNextRandom\" 'setNextRandom) 'Async'
                ])
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
functions. The only slightly tedious thing was to define the 'statefulExports'
field because it is a list of triples which has a list of exported
functionalities as its third argument. Another noteworthy detail, in case you
are not familiar with it, is the use of 'liftIO' in front of 'newStdGen'. You
have to do this, because 'newStdGen' has type @'IO' 'StdGen'@ but the actions
inside the startup code are of type
@'Neovim' ('StartupConfig' 'NeovimConfig') () something@. 'liftIO' lifts an
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
