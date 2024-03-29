# 2.3.1.0

* Add `subscribe` and `unsubscribe` function. Neovim doesn't automatically send
  event notifications to nvim-hs (or any other remote plugin) and for the
  callback of the `subscribe` funtion to trigger, you have to call a specific
  function before (e.g. `nvim_buf_attach`). In any case, if you want to subscribe 
  to a specific event, you have to read the documentation of the neovim 
  documentation. Some events are still better handled with autocommands.

# 2.3.0.0

* Windows is now rudimentarily supported. Since I couldn't find a library to
  connect to named pipes on windows and I didn't want to extend or write one,
  you have to use TCP sockets or Standard in and out to communicate with
  neovim. If you start neovim with `nvim --listen localhost:`, it will set the
  `NVIM` environment variable, so that nvim-hs can automatically connect to the
  neovim instance without passing any arguments.

# 2.2.0.0

* NeovimException are now thrown from (synchronous) remote functions and are no
  longer suppressed with an `error` call that also had a terrible error message.
  A function `catchNeovimException` (specialized `catch`) has been added that 
  catches these errors. 
* The return type of asynchronous functions is now alwas 
  `STM (Either NeovimException result)` and errors have to be handled by the 
  caller explicitly.

# 2.1.0.2

* Exported functions and commands now can have the same name.

# 2.1.0.0

* Autocommands now take an additional parameter of type `Synchronous`, allowing
  them to execute synchronous (previously hardcoded as `Async`).
  In order to adapt to this, change ` $(autocmd 'handler) opts` to
  `$(autocmd 'handler) Async opts`.

# 2.0.0.0

* Your configuration is now just a Haskell project. The dependency to Dyre has
  been removed and you are now forced to write a line of vimL and add a normal
  nvim-plugin to your setup. The template still does set everything up for you.
  The distinction between a plugin and a personal nvim-hs configuration is now
  gone and apart from settings things up, nothing has changed in this regard.
  The nvim-plugin contains the necessary documentation on what to do and
  should be available with `:help nvim-hs` when installed correctly.

* Since basically all generated functions were throwing exceptions anyway, the
  primed functions have become the default and if you want to explicitly handle
  error cases, you have to surround your code with `catch` or something more
  appropriate from `UnliftIO.Exception`. You have to remove `'` from your API
  calls or you have to adjust your error handling.

* There are now three flavors of APIs and you have to import one additionally to
  importing the `Neovim` module:

  - *Neovim.API.Text*: This uses strict `Text` for strings and `Vector` as
    lists. This is the recommended API to use.

  - *Neovim.API.String*: This is the same as before. Strings are `String`
    and lists are `[]`. This is for the lazy and backwards-ish compatiblity.

  - *Neovim.API.ByteString*: This can be useful for really performance critical
    stuff or if you`re writing a plugin for binary files.

# 1.0.1.0

* The `Neovim.Debug` module is now more pleasant to use.

# 1.0.0.2

* With the api of neovim 0.3.0, a function was exposed that had a reserved
  haskell keyword as a parameter name. The code generation did not sanitize
  this. This bugfix releases resolves this.

# 1.0.0.0

* Each plugin (type) now defines an environment which is similar to how
  stateful plugins have been declared in previous versions. If you need
  multiple different environments for different functions, you can make them
  fields of a bigger environment or define multiple plugin values.

  The type `Neovim r st a` has become `Neovim env a` where `env` is
  technically equivalent to the previous `r`.
  I was mainly motivated by this blog post:

  https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

* Only works with ghc >= 8. I removed some backwards compatibility. If you
  need older ghc versions, just use the previous version (0.2.5) as the
  feature set hasn't really changed.

* A different pretty printer library is now used and may surface at some
  places.

* Functions do now time out after some time, 10 seconds for those that block
  neovim and 10 minutes for background functions.

* A few types have been adjusted.

* Some improvement in error reporting.

# 0.2.5

* Older versions of `nvim-hs` may not function if some versions of a
  dependency are used. This version has proper bounds for the dependency and
  should cause a compile time failure if an incompatible version of the
  dependency is used (see #61).

# 0.2.0

* Replace error code of remote functions to return
  `Either NeovimException a` instead of a generic messagepack `Object`

* Export API functions that throw a `NeovimException` instead of returning
  `Either NeovimExeception a`.

* Replace three element tuple for stateful function declaration (#53)

* Add a stack template for easier setup

* Exceptions from pure code are now caught (#48)

# 0.1.0

* Adjust parser for output of `nvim --api-info`

* Adjust parser of ConfigHelper plugin

# 0.0.7

* Adjust handling of string sent by neovim in API generation.

# 0.0.6

* Noteworthy new API functions for the user's convenience:

  - `errOnInvalidResult`
  - `(:+)`

* ansi-wl-pprint is used for pretty printing of various things now. Most
  notably, the error type has been changed from `String` to `Doc`.
  This is a breaking change, but it was kind of announced in the issues
  list. In any case, uses of `err` can be fixed by enabling the
  `OverloadedStrings` extension. Other breakages have to be fixed by hand.

# 0.0.5

* Documentation received some love.

* A few renames of repurposed internals.

# 0.0.3

* Debugging facilities for ghci have been added. Check out the
  `Neovim.Debug` module! These few functions are very valuable to debug your
  code or even the code of nvim-hs itself.

* Startup code now has a special `Neovim` environment which has access to
  some of the internals that may or may not be useful. This change allowed
  the ConfigHelper plugin to be included as a normal, separable plugin.
  Unfortunately, this potentially breaks the plugin startup code of some
  existing plugins.

* Neovim context is no longer a type synonym, but a newtype wrapper around
  the previous type synonym with an added `ResourceT` wrapper. The functions
  from `MonadReader` are now actually exported as those.

  As a consequence, some of your code may break if you lack some specific
  instances which were auto-derived before. Send a PR or open a ticket to
  resolve this.

* Add handling for some kind of variadic arguments handling.

  A command or function will be passed `Nothing` as it's
  last arguments if the argument type is wrapped in `Maybe`
  and the invocation on the side of neovim did not pass those
  arguments.

# 0.0.2

* Add handling for special command options

  This breaks code that used `command` or `command'` to export
  functionality. You should replace the options with a list
  of `CommandOptions`.

  An export like `$(command' foo) def { cmdSync = Async }` must be redefined
  to `$(command' foo) [CmdSync Async]`.

# 0.0.1

* Usable prototype implementation
