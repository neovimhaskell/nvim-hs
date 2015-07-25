" This script tests the intoroperability with all plugins defined in
" TestPlugins.hs
"
" TODO Define functions that return error messages for the assertions.
" For now, if the tests fail you should check the test log file for error
" messages (something like dist/test/nvim-hs-0.0.1-hspec.log).

" Initialize the plugin provider
call remote#host#Register('test', "*.l\?hs", rpcstart('./TestPlugins.sh', ['test']))
let haskellChannel = remote#host#Require('test')
" We need to issue a synchornous request to make sure that the function
" hooks are registered before we try to call them. Issueing this as late as
" possible will improve startup time because this is a blocking operation.
" TODO think about hooking into a general undefined function autocmd.
try
	if "Pong" != rpcrequest(haskellChannel, 'PingNvimhs', [])
		echom 'Ping function not properly registered'
		cq!
	endif
catch
	echom 'Functions not properly registered aborting'
	cq!
endtry

if haskellChannel < 1
	echom 'Failure to initialize the haskell channel for remote procedure calls'
	cq!
endif

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

