{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module PromptGen (makePrompt) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Random ( fromList, MonadRandom )
import Control.Monad.State ( MonadState(get), evalStateT )
import Control.Monad.Trans.State ()

import Data.Char (toLower, toUpper)
import qualified Data.Map as M
import qualified Data.List as L

---------------------------------------------------------------------------------------------------
-- | Types
---------------------------------------------------------------------------------------------------
type ST = [Char]
type MonadPrompt m = (MonadState ST m, MonadRandom m)

---------------------------------------------------------------------------------------------------
-- | Constants
---------------------------------------------------------------------------------------------------
-- English letter frequencies based on
-- https://pi.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
letterFreqs :: M.Map Char Rational
letterFreqs = M.fromList
    [ ('E', 12.02)
    , ('T', 9.10)
    , ('A', 8.12)
    , ('O', 7.68)
    , ('I', 7.31)
    , ('N', 6.95)
    , ('S', 6.28)
    , ('R', 6.02)
    , ('H', 5.92)
    , ('D', 4.32)
    , ('L', 3.98)
    , ('U', 2.88)
    , ('C', 2.71)
    , ('M', 2.61)
    , ('F', 2.30)
    , ('Y', 2.11)
    , ('W', 2.09)
    , ('G', 2.03)
    , ('P', 1.82)
    , ('B', 1.49)
    , ('V', 1.11)
    , ('K', 0.69)
    , ('X', 0.17)
    , ('Q', 0.11)
    , ('J', 0.10)
    , ('Z', 0.07)
    ]

-- default frequency for map lookup
defFreq :: Rational
defFreq = 0.01

lowercaseLetters :: [Char]
lowercaseLetters = ['a'..'z']

homeRowLetters :: [Char]
homeRowLetters = "asdfghjkl;"

---------------------------------------------------------------------------------------------------
-- | Helper Functions
---------------------------------------------------------------------------------------------------
chooseLen :: (MonadPrompt m) => [Int] -> (Float -> Float) -> m Int
chooseLen range f = do
    let freqMap = fmap (toRational . f . fromIntegral) range
    fromList $ zip range freqMap

sampleMany :: (MonadPrompt m) => m a -> Int -> m [a]
sampleMany sampler = go
    where
        go 0 = return []
        go n = do
            c  <- sampler
            cs <- go (n-1)
            return (c:cs)

---------------------------------------------------------------------------------------------------
-- | Choose Word and Sentence Lengths
--   based on frequency distribution approximations in
--   https://math.wvu.edu/~hdiamond/Math222F17/Sigurd_et_al-2004-Studia_Linguistica.pdf 
---------------------------------------------------------------------------------------------------
chooseWordLen :: (MonadPrompt m) => m Int
chooseWordLen = chooseLen lengths freqs
    where
        lengths = [3..12]
        freqs l = 11.74 * l**3 * 0.4**l   -- 21.2 * 0.73 ** (l - 3)

chooseSentLen :: (MonadPrompt m) => m Int
chooseSentLen = chooseLen lengths freqs
    where
        lengths = [1..50]
        freqs l = 1.1 * l * 0.9**l

---------------------------------------------------------------------------------------------------
-- | Sample One or Multiple Letters
---------------------------------------------------------------------------------------------------
sampleLetter :: (MonadPrompt m) => m Char
sampleLetter = do
    cs <- Control.Monad.State.get
    let letters = [(toLower c, M.findWithDefault defFreq (toUpper c) letterFreqs) | c <- cs]
    fromList letters

sampleLetters :: (MonadPrompt m) => Int -> m String
sampleLetters = sampleMany sampleLetter

---------------------------------------------------------------------------------------------------
-- | Construct Words and Sentences
---------------------------------------------------------------------------------------------------
makeRandWord :: (MonadPrompt m) => m String
makeRandWord = do
    l <- chooseWordLen
    sampleLetters l

sampleWords :: (MonadPrompt m) => Int -> m [String]
sampleWords = sampleMany makeRandWord

makeRandSent :: (MonadPrompt m) => m String
makeRandSent = do
    l  <- chooseSentLen
    ws <- sampleWords l
    let sent = unwords ws ++ "."
    let c:cs = sent
    return (toUpper c : cs)

makeParagraph :: (MonadPrompt m) => m String 
makeParagraph = do
    s0 <- makeRandSent 
    s1 <- makeRandSent 
    s2 <- makeRandSent
    s3 <- makeRandSent
    s4 <- makeRandSent
    s5 <- makeRandSent
    s6 <- makeRandSent 
    s7 <- makeRandSent
    s8 <- makeRandSent
    s9 <- makeRandSent
    return $ unwords [s0, s1, s2, s3, s4, s5, s6, s7, s8, s9]

makePrompt :: [Char] -> IO String
makePrompt = do evalStateT makeParagraph
