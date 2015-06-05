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
if randomValue != 42
	echom 'Expected Value of 42 but got: ' . randomValue
	cq!
endif
let randomValue = rpcrequest(haskellChannel, "Random")
if randomValue != 17
	echom 'Expected Value of 17 but got: ' . randomValue
	cq!
endif
let randomValue = rpcrequest(haskellChannel, "Random")
if randomValue != -666
	echom 'Expected Value of -666 but got: ' . randomValue
	cq!
endif

" This call should throw an error, but it shouldn't cause any problems for
" the execution.
let randoomValue = rpcrequest(haskellChannel, "Randoom")

" Everything works, exit normally
q!

