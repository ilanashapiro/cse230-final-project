{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module WordAnalysis (getTopThree,
                     sortedMostMissedLetters,
                     sortedListMissedWords,
                     testIsSortedMissedWords,
                     testIsSortedMissedLetters
                    ) where

import qualified Data.Map.Strict as Map
import Data.List ( sortOn, nub, groupBy, sortBy )
import Data.Ord ( Down(Down) )
import Data.Function (on)
import Test.QuickCheck ( Property, (==>), Gen, Arbitrary, Arbitrary (arbitrary), listOf )

-- converts the map into a list of missed words in descending order of how often they're missed
sortedListMissedWords :: Map.Map String Int -> [(String, Int)]
sortedListMissedWords m = sortOn (Down . snd) (Map.toList m)

sortedMostMissedLetters :: Map.Map String Int -> [(Char, Int)]
sortedMostMissedLetters m
    | null m = []
    | otherwise = sortOn (Down . snd) (go m l)
  where
    l = sortedListMissedWords m
    go :: Map.Map String Int -> [(String, Int)] -> [(Char, Int)]
    go m [] = []
    go m (x : xs) = do
      let letters = getletters x
      let ltrFreqMap = zip letters (map (getLetterFreqInWord (fst x)) letters)
      let wordFreq = m Map.! fst x
      let ltrOverallFreqList = zip letters (map ((* wordFreq) . snd) ltrFreqMap)
      combineLists ltrOverallFreqList (go m xs)

-- >>> sortedMostMissedLetters (Map.fromList [("Aba", 5), ("bb", 3)])
-- [('b',11),('A',5),('a',5)]

getletters :: (String, Int) -> [Char]
getletters x = nub (fst x)

combineLists :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
combineLists list1 list2 =
  let combined = groupBy ((==) `on` fst) (sortBy (compare `on` fst) (list1 ++ list2))
  in map (\group -> (fst (head group), sum (map snd group))) combined

getLetterFreqInWord :: String -> Char -> Int
getLetterFreqInWord "" c = 0
getLetterFreqInWord (s : str) c
  | c == s = 1 + getLetterFreqInWord str c
  | otherwise = getLetterFreqInWord str c

getTopThree :: Map.Map String Int -> (Map.Map String Int -> [(a, Int)] ) -> [a]
getTopThree m f
    | length l >= 3 = [fst (head l), fst (head (tail l)), fst (head (tail (tail l)))]
    | length l == 2 = [fst (head l), fst (head (tail l))]
    | length l == 1 = [fst (head l)]
    | null l        = []
  where l = f m


testIsSortedMissedWords :: Map.Map String Int -> Bool
testIsSortedMissedWords m = isSortedDescending (sortedListMissedWords m)

testIsSortedMissedLetters :: Map.Map String Int -> Bool
testIsSortedMissedLetters m = isSortedDescending (sortedMostMissedLetters m)

isSortedDescending :: (Ord k) => [(k, Int)] -> Bool
isSortedDescending [] = True
isSortedDescending [_] = True
isSortedDescending ((_, x):rest@((_, y) : _)) = x >= y && isSortedDescending rest

-- Generators
genKeyValue :: Gen (String, Int)
genKeyValue = do
  key <- arbitrary
  value <- arbitrary
  return (key, value)

genStringIntMap :: Gen (Map.Map String Int)
genStringIntMap = do
  keyValues <- listOf genKeyValue
  return (Map.fromList keyValues)

-- Define an instance of Arbitrary for Map
instance {-# OVERLAPPING #-} Arbitrary (Map.Map String Int) where
  arbitrary :: Gen (Map.Map String Int)
  arbitrary = genStringIntMap



