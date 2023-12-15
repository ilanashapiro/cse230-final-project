module Main where

import RandomPromptGen (makeRandPrompt)
import BigramPromptGen (makePrompt)

main :: IO ()
main = do
    print "RANDOM LETTER PROMPT"
    sRandom <- makeRandPrompt ['a'..'z']
    print sRandom
    print ""

    print "SHAKESPEARE STYLE PROMPT"
    sBigram <- makePrompt "training-text/shakespeare.txt"
    print sBigram
    print ""
    
    print "HASKELL PROMPT"
    sBigram <- makePrompt "app/Main.hs" --"src/RandomPromptGen.hs"
    print sBigram
    print ""
