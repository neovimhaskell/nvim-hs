# saep's setup with a single cabal sandbox

I'm working on `nvim-hs` with the following setup:

* `nvim-hs` repository cloned to `~/git/nvim-hs`

* cabal sandbox inside that directory

* script from the repository is used to start `nvim-hs` from within neovim

  ```vim
if has('nvim')
  function! s:RequireHaskellHost(name)
    return rpcstart("/home/saep/.bin/nvim-hs-devel.sh", ['-l','/tmp/nvim-log.txt','-v','DEBUG',a:name.name])
  endfunction

  call remote#host#Register('haskell', "*.l\?hs", function('s:RequireHaskellHost'))
  let hc=remote#host#Require('haskell')
  call rpcrequest(hc, "PingNvimhs")
endif
  ```

* I edit `~/.config/nvim/nvim.hs` etc. with a neovim instance started from
  `~/git/nvim-hs` because of some tools/plugins I use.

* I install dependencies I need for my local plugins (those inside
  `~/.config/nvim/lib` into the sandbox

Plugins (plug syntax):

```
Plug 'eagletmt/ghcmod-vim', { 'for': ['haskell','lhaskell','chaskell'] }
Plug 'eagletmt/neco-ghc', { 'for': ['haskell','lhaskell','chaskell']}
Plug 'saep/hasksyn', { 'for': ['haskell','lhaskell','chaskell']}
```

Tools:

* ghc-mod from master (version edited by hand)
* codex + hasktags (for tags)

