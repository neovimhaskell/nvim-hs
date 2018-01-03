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
