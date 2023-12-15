{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <$>" #-}

module ImageGen (
    ImageDocs, 
    partialImage, 
    getRandomImageDocs, 
    testPartialDocShape,
    testPartialDocIllegal,
    testDocSmallCur, 
    testDocLargeCur, 
    ) where

import System.Directory (listDirectory)
import System.FilePath.Posix 
import Control.Monad.Random 
import Graphics.Vty as V
import qualified Data.IMap as V
import GHC.Profiling (stopHeapProfTimer)
import Test.QuickCheck (Gen, Arbitrary (arbitrary), elements, (==>), Property)
import Test.QuickCheck.Gen (genFloat)


data Doc a = D [[a]] deriving (Eq, Show)

data GrowthMethod   = LineByLineT2B
                    | RandGrowth
                    deriving (Show)

docFromPath :: FilePath -> IO (Doc Char)
docFromPath path = do 
    contents        <- readFile path
    let fileLines   = lines contents
    return $ D fileLines

defaultAttr :: Attr
defaultAttr = V.Attr V.Default V.Default V.Default V.Default


data ImageDocs = IDocs {
    growth  :: GrowthMethod, 
    doc :: Doc Char,
    randMask :: [[Float]]
} deriving (Show)


getRandRow :: [a] -> Gen [Float]
getRandRow []       = return []
getRandRow (el:els) = do
    elVal <- genFloat 
    elVals  <- getRandRow els
    return $ elVal : elVals 


getRandMat :: [[Char]] -> Gen [[Float]]
getRandMat []       = return []
getRandMat (r:rs)   = do 
    randRow <- getRandRow r
    randMat <- getRandMat rs
    return $ randRow : randMat 


instance Arbitrary ImageDocs where
    arbitrary = do
        method  <- elements [RandGrowth, LineByLineT2B]
        cList   <- arbitrary 
        mask    <- getRandMat cList 
        return $ IDocs method (D cList) mask


maskRow :: [a] -> a -> [Bool] -> [a]
maskRow [] _ _              = []
maskRow r  _ []             = r
maskRow (el:els) def (m:ms) = maskedEl : maskRow els def ms
    where maskedEl = if m then el else def


maskMatrix :: [[a]] -> a -> [[Bool]] -> [[a]]
maskMatrix [] _ _             = [] 
maskMatrix m  _ []            = m
maskMatrix (r:rs) def (m:ms)  = maskedRow : maskMatrix rs def ms
    where maskedRow = maskRow r def m



maskedDoc :: Doc Char -> [[Bool]] -> Doc Char 
maskedDoc (D cs) m = D maskedLines 
    where 
        maskedLines = maskMatrix cs ' ' m 


fullLike :: [a] -> b -> [b]
fullLike [] _       = []
fullLike (a:as) b   = b : fullLike as b

fullLike2D :: [[a]] -> b -> [[b]]
fullLike2D []     _ = []
fullLike2D (a:as) b = fullLike a b : fullLike2D as b

sameShape :: [[a]] -> [[b]] -> Bool
sameShape [] []     = True
sameShape [] _      = False
sameShape _  []     = False
sameShape (a:as) (b:bs) = length a == length b && sameShape as bs

docSameShape :: Doc a -> Doc b -> Bool
docSameShape (D a) (D b) = sameShape a b

fullDocLike :: Doc a -> b -> Doc b
fullDocLike (D as) b = D (fullLike2D as b)


firstNMask :: [[a]] -> Int -> [[Bool]]
firstNMask []    _ = []
firstNMask (r:rs) 0 = fullLike r False : firstNMask rs 0
firstNMask (r:rs) n = fullLike r True  : firstNMask rs (n-1)


docToImage :: Doc Char -> V.Image
docToImage (D lines) = V.vertCat imageLines
    where imageLines = map (V.string defaultAttr) lines


partialDocLBLT2B :: Doc Char -> Int -> Int -> Doc Char 
partialDocLBLT2B doc@(D lines) cur stop = maskedDoc doc mask
    where 
        numLines = length lines * cur `div` stop
        mask     = firstNMask lines numLines 


partialDocRand :: Doc Char -> [[Float]] -> Int -> Int -> Doc Char 
partialDocRand doc@(D lines) floatMask cur stop = maskedDoc doc mask
    where 
        f el    = el * fromIntegral stop <= fromIntegral cur
        mask    = map (map f) floatMask

testDocSmallCur :: ImageDocs -> Int -> Int -> Property
testDocSmallCur iDocs cur stop = 0 < stop && cur <= 0 ==> (partial == emp)
    where 
        partial = partialDoc iDocs cur stop
        emp     = fullDocLike (doc iDocs) ' '

testDocLargeCur :: ImageDocs -> Int -> Int -> Property
testDocLargeCur iDocs cur stop = 0 < stop && stop <= cur ==> (partial == doc iDocs)
    where 
        partial = partialDoc iDocs cur stop


testPartialDocShape :: ImageDocs -> Int -> Int -> Bool
testPartialDocShape iDocs cur stop = docSameShape orig partial
    where
        orig    = doc iDocs
        partial = partialDoc iDocs cur stop

testPartialDocIllegal :: ImageDocs -> Int -> Int -> Property
testPartialDocIllegal iDocs cur stop = (cur < 0 || stop <= 0) ==> (partial == emp)
    where
        partial = partialDoc iDocs cur stop
        emp     = fullDocLike (doc iDocs) ' '


partialDoc :: ImageDocs -> Int -> Int -> Doc Char
partialDoc iDocs cur stop 
    | stop <= 0 || cur < 0  = fullDocLike (doc iDocs) ' '
    | otherwise             = partialByMethod iDocs cur stop
        where 
            partialByMethod (IDocs LineByLineT2B d _)    = partialDocLBLT2B d
            partialByMethod (IDocs RandGrowth d g)       = partialDocRand d g 


partialImage :: ImageDocs -> Int -> Int -> V.Image
partialImage iDocs cur stop = docToImage $ partialDoc iDocs cur stop


getRandomGrowth :: IO GrowthMethod
getRandomGrowth = do
    let growths     = [LineByLineT2B]
    idx             <- getRandomR (0, length growths - 1)
    return $ growths !! idx 

getMatchingRandRow :: [a] -> IO [Float]
getMatchingRandRow []       = return []
getMatchingRandRow (el:els) = do
    elVal <- randomIO 
    elVals  <- getMatchingRandRow els
    return $ elVal : elVals 


getMatchingRandMat :: [[Char]] -> IO [[Float]]
getMatchingRandMat []       = return []
getMatchingRandMat (r:rs)   = do 
    randRow <- getMatchingRandRow r
    randMat <- getMatchingRandMat rs
    return $ randRow : randMat 


getRandomImageDocs :: String -> IO ImageDocs
getRandomImageDocs dirPath = do
    imgFiles    <- liftIO $ listDirectory dirPath
    idx         <- getRandomR (0, length imgFiles - 1)
    let path    = dirPath </> imgFiles !! idx 
    doc@(D rs)  <- liftIO $ docFromPath path
    randMask    <- getMatchingRandMat rs
    --growth      <- getRandomGrowth
    let growth  = RandGrowth
    return $ IDocs growth doc randMask


