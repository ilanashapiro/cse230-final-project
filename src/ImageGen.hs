{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module ImageGen (ImageDocs, partialImage, getRandomImageDocs) where

import System.Directory (listDirectory)
import Control.Monad.Random 
import Graphics.Vty as V
import qualified Data.IMap as V


data Doc a = D [[a]] deriving (Show)

data GrowthMethod   = LineByLineT2B
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
    doc :: Doc Char
} deriving (Show)

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

maskedImage :: Doc Char -> [[Bool]] -> V.Image 
maskedImage (D cs) m = V.vertCat imageLines 
    where 
        maskedLines = maskMatrix cs ' ' m 
        imageLines  = map (V.string defaultAttr) maskedLines


fullLike :: [a] -> b -> [b]
fullLike [] _       = []
fullLike (a:as) b   = b : fullLike as b


firstNMask :: [[a]] -> Int -> [[Bool]]
firstNMask []    _ = []
firstNMask (r:rs) 0 = fullLike r False : firstNMask rs 0
firstNMask (r:rs) n = fullLike r True  : firstNMask rs (n-1)


partialImgLBLT2B :: Doc Char -> Int -> Int -> V.Image 
partialImgLBLT2B doc@(D lines) cur stop = maskedImage doc mask
    where 
        numLines = length lines * cur `div` stop
        mask     = firstNMask lines numLines 

partialImage :: ImageDocs -> Int -> Int -> V.Image
partialImage (IDocs LineByLineT2B d) = partialImgLBLT2B d 


getRandomGrowth :: (MonadRandom m) => m GrowthMethod
getRandomGrowth = do
    let growths     = [LineByLineT2B]
    idx             <- getRandomR (0, length growths)
    return $ growths !! idx 


getRandomImageDocs :: (MonadRandom m, MonadIO m) => String -> m ImageDocs
getRandomImageDocs dirPath = do
    imgFiles    <- liftIO $ listDirectory dirPath
    idx         <- getRandomR (0, length imgFiles)
    let path    = imgFiles !! idx 
    doc         <- liftIO $ docFromPath path
    growth      <- getRandomGrowth
    return $ IDocs growth doc

