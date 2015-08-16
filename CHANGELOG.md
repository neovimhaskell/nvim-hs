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
