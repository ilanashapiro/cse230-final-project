module Main where

import PromptGen (makePrompt)

main :: IO ()
main = do
    s <- makePrompt ['a' .. 'z']
    print s
