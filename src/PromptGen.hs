module Main where

import Control.Monad.Random (fromList)
import Data.Char (toLower, toUpper)
import qualified Data.Map as M
import qualified Data.List as L

---------------------------------------------------------------------------------------------------
-- | Constants
---------------------------------------------------------------------------------------------------
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

homeRowLetters :: [Char]
homeRowLetters = ['a','s','d','f','g','h','j','k','l']

---------------------------------------------------------------------------------------------------
-- | Helper Functions
---------------------------------------------------------------------------------------------------
chooseLen :: [Int] -> (Float -> Float) -> IO Int
chooseLen range f = do
    let freqMap = fmap (toRational . f . fromIntegral) range 
    l <- fromList $ zip range freqMap
    return l 

sampleMany :: IO a -> Int -> IO [a]
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
chooseWordLen :: IO Int
chooseWordLen = chooseLen lengths freqs
    where
        lengths = [3..12] 
        freqs l = 11.74 * l**3 * 0.4**l   -- 21.2 * 0.73 ** (l - 3)

chooseSentLen :: IO Int
chooseSentLen = chooseLen lengths freqs
    where
        lengths = [1..50]
        freqs l = 1.1 * l * 0.9**l

---------------------------------------------------------------------------------------------------
-- | Sample Letters
---------------------------------------------------------------------------------------------------
sampleLetter :: [Char] -> IO Char
sampleLetter cs = do
    let letters = [(toLower c, M.findWithDefault 0 (toUpper c) letterFreqs) | c <- cs] 
    c <- fromList letters
    return c       

sampleLetterAny :: IO Char
sampleLetterAny = sampleLetter ['a'..'z']

sampleLetters :: Int -> IO String
sampleLetters = sampleMany (sampleLetter homeRowLetters)

---------------------------------------------------------------------------------------------------
-- | Construct Words and Sentences
---------------------------------------------------------------------------------------------------
makeRandWord :: IO String
makeRandWord = do 
    l <- chooseWordLen
    sampleLetters l

sampleWords :: Int -> IO [String]
sampleWords = sampleMany makeRandWord

makeRandSent :: IO String
makeRandSent = do
    l  <- chooseSentLen
    ws <- sampleWords l
    let sent = unwords ws ++ "."
    let c:cs = sent
    return $ (toUpper c) : cs

main :: IO ()
main = do 
    s <- makeRandSent
    putStrLn s
    