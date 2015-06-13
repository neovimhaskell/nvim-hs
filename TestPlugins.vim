" This script tests the intoroperability with all plugins defined in
" TestPlugins.hs
"
" TODO Define functions that return error messages for the assertions.
" For now, if the tests fail you should check the test log file for error
" messages (something like dist/test/nvim-hs-0.0.1-hspec.log).

" Initialize the plugin provider
function! s:RequireHaskellHost(name)
	return rpcstart("./nvim-hs.sh", [a:name])
endfunction

call remote#host#Register('nvim-hs-test', function('s:RequireHaskellHost'))
call remote#host#Require('nvim-hs-test')
if haskellChannel < 1
	" If the channel id is not a positive integer, something has gone wrong.
	echom 'Initialzing the plugin provider failed'
	cq!
endif

" The test plugin random number generator is initialized with a static seed
let randomValue = Random()
if randomValue != 42
	echom 'Expected Value of 42 but got: ' . randomValue
	cq!
endif
let randomValue = Random()
if randomValue != 17
	echom 'Expected Value of 17 but got: ' . randomValue
	cq!
endif
let randomValue = Random()
if randomValue != -666
	echom 'Expected Value of -666 but got: ' . randomValue
	cq!
endif

" Test notifications
call InjectNumber(7)
let randomValue = Random()
if randomValue != 7
	echom 'Expected Value of 7 but got: ' . randomValue
	cq!
endif


let notsorandomvalue = Const42()
if notsorandomvalue != 42
	echom 'Expected the ultimate answer, but got: ' . notsorandomvalue
	cq!
endif

" Everything works, exit normally
q!

