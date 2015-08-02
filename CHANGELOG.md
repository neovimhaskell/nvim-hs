# 0.0.3

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
