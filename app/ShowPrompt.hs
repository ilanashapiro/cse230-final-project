{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import RandomPromptGen (makeRandPrompt)
import BigramPromptGen (makePrompt)


main :: IO ()
main = do
    sRandom <- makeRandPrompt ['a'..'z']
    print sRandom
    sBigram <- makePrompt "training-text/shakespeare.txt"
    print sBigram
