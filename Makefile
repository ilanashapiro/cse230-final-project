run: 
	cabal run --ghc-options=-threaded cse230-final-project

run-image:
	cabal run --ghc-options=-threaded test-image

prompt:
	cabal run --ghc-options=-threaded test-prompt
	
test: 
	cabal run --ghc-options=-threaded test
