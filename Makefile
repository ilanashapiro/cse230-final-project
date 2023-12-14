
run: 
	cabal run --ghc-options=-threaded cse230-final-project

run-image:
	cabal run --ghc-options=-threaded test-image

test: 
	cabal run --ghc-options=-threaded test
