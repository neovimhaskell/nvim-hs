# 0.0.2

* Add handling for special command options

  This breaks code that used `command` or `command'` to export
  functionality. You should replace the options with a list
  of `CommandOptions`.

  An export like `$(command' foo) def { cmdSync = Async }` must be redefined
  to `$(command' foo) [CmdSync Async]`.

# 0.0.1

* Usable prototype implementation
