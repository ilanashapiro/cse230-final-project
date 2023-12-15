{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module BigramPromptGen where

import Control.Monad.State (withState, MonadState (get, put), evalStateT)
import qualified Data.Map as M
import qualified Data.List as L
import Data.List (group)
import Control.Monad.Random (MonadRandom, fromList)

type ST = (M.Map String (M.Map String Rational), [String], String)
type MonadPrompt m = (MonadState ST m, MonadRandom m)

-- | Function for creating a list of bigrams
-- | e.g. [("Colorless", "green"), ("green", "ideas")]
-- | source: https://stackoverflow.com/questions/33646178/haskell-finding-bigrams-from-an-input-list-of-words
getBigrams :: [String] -> [(String, String)]
getBigrams ws = do
    w1 <- init ws
    w2 <- tail ws
    return (w1, w2)

-- | Get frequency of unigrams from list of words
freqUnigrams :: [String] -> M.Map String Rational
freqUnigrams = L.foldl' countElems M.empty
    where
        countElems mp k = M.insertWith (+) k 1 mp

-- | Get frequency of bigrams from list of bigrams
freqBigrams :: [String] -> M.Map String (M.Map String Rational)
freqBigrams ws = L.foldl' countElems M.empty (getBigrams ws)
    where
        countElems mp (k1, k2) = M.insertWith (\_ oldMp -> M.insertWith (+) k2 1 oldMp) k1 (M.singleton k2 1) mp

genNextWord :: (MonadPrompt m) => m [String]
genNextWord = do
    (bigrams, sent, last) <- get
    let candidates = M.toList $ M.findWithDefault M.empty last bigrams
    next <- Control.Monad.Random.fromList candidates
    put (bigrams, next : sent, next)
    return (next : sent)

genSentence :: (MonadPrompt m) => m String
genSentence = go
    where
        go = do
            s <- genNextWord
            if last (head s) == '.' then
                return $ unwords (reverse s)
            else
                go

makePrompt :: String -> IO String
makePrompt fpath = do
    ws               <- readFile fpath
    let wordList     = words ws
    let bigrams      = freqBigrams wordList
    fstWord          <- fromList $ M.toList (freqUnigrams wordList)
    let initialState = (bigrams, [fstWord], fstWord)
    evalStateT genSentence initialState
