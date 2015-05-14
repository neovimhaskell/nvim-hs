" This script tests the intoroperability with all plugins defined in
" TestPlugins.hs
"
" TODO Define functions that return error messages for the assertions.
" For now, if the tests fail you should check the test log file for error
" messages (something like dist/test/nvim-hs-0.0.1-hspec.log).

" Initialize the plugin provider
let haskellChannel = rpcstart('./nvim-hs.sh', [])
if haskellChannel < 1
	" If the channel id is not a positive integer, something has gone wrong.
	echom 'Initialzing the plugin provider failed'
	cq!
endif

" The test plugin random number generator is initialized with a static seed
let randomValue = rpcrequest(haskellChannel, "Random")
if randomValue != 8741
	echom 'Expected Value of 8741 but got: ' . randomValue
	cq!
endif
let randomValue = rpcrequest(haskellChannel, "Random")
if randomValue != 8101
	echom 'Expected Value of 8101 but got: ' . randomValue
	cq!
endif
let randomValue = rpcrequest(haskellChannel, "Random")
if randomValue != -22565
	echom 'Expected Value of -22565 but got: ' . randomValue
	cq!
endif

" Everything works, exit normally
q!

