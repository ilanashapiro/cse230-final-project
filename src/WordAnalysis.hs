{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module WordAnalysis (getTopThree) where

import qualified Data.Map.Strict as Map
import Data.List ( sortOn )
import Data.Ord ( Down(Down) )

-- converts the map into a list of missed words in descending order of how often they're missed
sortedListMissedWords :: Map.Map String Int -> [(String, Int)]
sortedListMissedWords m = sortOn (Down . snd) (Map.toList m)

getTopThree :: Map.Map String Int -> [String]
getTopThree m
    | length l >= 3 = [fst (head l), fst (head (tail l)), fst (head (tail (tail l)))]
    | length l == 2 = [fst (head l), fst (head (tail l))]
    | length l == 1 = [fst (head l)]
    | null l        = []
  where l = sortedListMissedWords m



