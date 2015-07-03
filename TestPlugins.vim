" This script tests the intoroperability with all plugins defined in
" TestPlugins.hs
"
" TODO Define functions that return error messages for the assertions.
" For now, if the tests fail you should check the test log file for error
" messages (something like dist/test/nvim-hs-0.0.1-hspec.log).

" Initialize the plugin provider
let s:hostName = 'test'
function! s:RequireHaskellHost(name)
	return rpcstart("./nvim-hs.sh", [a:name])
endfunction

call remote#host#Register(s:hostName, ".l\?hs",function('s:RequireHaskellHost'))
let haskellChannel = remote#host#Require(s:hostName)
echom haskellChannel
" FIXME A request must be made to the host to make the following calls work.
" It does not matter what function is called, just that some function must
" be called.
echom rpcrequest(haskellChannel, "test")

if haskellChannel < 1
	echom 'Failure to initialize the haskell channel for remote procedure calls'
	cq!
endif
echom remote#host#IsRunning(s:hostName)

" The test plugin random number generator is initialized with a static seed
if Random() != 42
	echom 'Expected Value of 42 but got: ' . Random()
	cq!
endif
if Random() != 17
	echom 'Expected Value of 17 but got: ' . Random()
	cq!
endif
if Random() != -666
	echom 'Expected Value of -666 but got: ' . Random()
	cq!
endif

" Test notifications
call InjectNumber(7)
if Random() != 7
	echom 'Expected Value of 7 but got: ' . Random()
	cq!
endif


let notsorandomvalue = Const42()
if notsorandomvalue != 42
	echom 'Expected the ultimate answer, but got: ' . notsorandomvalue
	cq!
endif

" Everything works, exit normally
q!

