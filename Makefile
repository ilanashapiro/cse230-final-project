run: 
	cabal run --ghc-options=-threaded cse230-final-project

random-mode: 
	cabal run --ghc-options=-threaded cse230-final-project -- random

shakespeare-mode: 
	cabal run --ghc-options=-threaded cse230-final-project -- shakespeare

image:
	cabal run --ghc-options=-threaded test-image

prompt:
	cabal run --ghc-options=-threaded test-prompt
	
test: 
	cabal run --ghc-options=-threaded test
