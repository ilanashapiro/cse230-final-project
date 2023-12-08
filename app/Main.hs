{-# LANGUAGE TemplateHaskell #-} -- For using lenses.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Main where

import Data.List (isPrefixOf)
import Data.Char (isSpace)
import Text.Printf
import Data.Time 
import Control.Monad.IO.Class (liftIO)
import Data.Maybe             (fromMaybe)

import Lens.Micro
import Lens.Micro.Mtl (zoom)
import Lens.Micro.TH ( makeLenses )
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Core as C
import Brick.Widgets.Core ( (<=>) )
import qualified Brick.Widgets.Edit as E 
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)
import Control.Exception (handle)
import qualified Brick as B
import Graphics.Vty (Key(KChar))

data EditorName = EName deriving (Eq, Ord, Show)

data State = ST { 
  _editor :: E.Editor String EditorName, 
  _numIncorrect :: Int,
  _numTypedWords :: Int,
  _lastCharIsSpace :: Bool,
  _startTimestamp :: Maybe UTCTime,
  _currTimestamp :: Maybe UTCTime,
  img :: V.Image
}
makeLenses ''State

successAttrName :: A.AttrName
successAttrName = A.attrName "success"

errorAttrName :: A.AttrName
errorAttrName = A.attrName "error"

defaultAttrName :: A.AttrName
defaultAttrName = A.attrName "default"

referenceText :: String
referenceText = "The sun dipped low on the horizon, casting a warm hue across the tranquil meadow.\
                \ A gentle breeze whispered through the swaying grass, carrying the sweet scent of wildflowers.\
                \ In the distance, a family of deer grazed peacefully, their graceful movements adding to the serene\
                \ symphony of nature. Birds soared overhead, painting the sky with fleeting strokes of vibrant colors\
                \ as they headed towards their roosts. The air was filled with a sense of calm, a moment frozen in\
                \ time where worries seemed to dissipate. It was a scene of simple beauty, a sanctuary inviting one\
                \ to pause and embrace the tranquility of the natural world"

coloredWordsWidget :: Bool -> String -> T.Widget EditorName
coloredWordsWidget lastCharIsSpace str 
  | null (dropWhile isSpace str)  = C.str ""
  | otherwise = foldl1 (C.<+>) $ map colorizeWord $ zip [0..] inputWords
    where
      inputWords = words str
      colorizeWord (idx, word) =
          let referenceWord = words referenceText !! idx
              -- if we didn't just end the word with a space
              -- and we're not on the current word being typed
              -- and the word isn't a prefix of the reference (i.e. it could still be correct
              -- so we are still in progress)
              attrName  
                | not lastCharIsSpace && (idx == length inputWords - 1) && (isPrefixOf word referenceWord)
                    = defaultAttrName
                | referenceWord == word 
                    = successAttrName 
                | otherwise = errorAttrName
          in C.withAttr attrName $ C.str (word ++ " ")

draw :: State -> [T.Widget EditorName]
draw st = [img' <=> e <=> wordCount] 
  where
      e                 = E.renderEditor ((coloredWordsWidget (st ^. lastCharIsSpace)) . concat) True (st ^. editor)
      img'              = C.raw (img st)
      numTotalWords     = st ^. numTypedWords
      numIncorrectWords = st ^. numIncorrect
      numCorrectWords   = numTotalWords - numIncorrectWords
      percentError      = if numTotalWords == 0 then 0 else (fromIntegral numCorrectWords) / (fromIntegral numTotalWords) * 100 :: Float
      spaces            = replicate 4 ' '
      wordCount         = foldl1 (C.<+>) [
                            C.withAttr defaultAttrName $ C.str ("Typed Words: " ++ show numTotalWords ++ spaces),
                            C.withAttr defaultAttrName $ C.str ("WPM: " ++ printf "%.2g" (wordsPerMin st)  ++ spaces),
                            C.withAttr defaultAttrName $ C.str ("Accuracy: " ++ printf "%.2g" percentError ++ "%" ++ spaces),
                            C.withAttr successAttrName $ C.str ("Correct: " ++ show numCorrectWords ++ spaces),
                            C.withAttr errorAttrName   $ C.str ("Errors: " ++ show numIncorrectWords) 
                          ]

updateState :: Bool -> UTCTime -> State -> State
updateState isLastCharSpace currTime st =
  let text = E.getEditContents $ st ^. editor
      prevNumIncorrect = st ^. numIncorrect
      wordList = words $ concat text -- Split text into words
      lastTypedWord = if null wordList then "" else last wordList
      referenceWord = words referenceText !! (length wordList - 1)
      wordIsIncorrect = (isLastCharSpace && lastTypedWord /= referenceWord)
      storedStartTime = st ^. startTimestamp
      startTime = if storedStartTime == Nothing then Just currTime else storedStartTime
      storedCurrTime = st ^. currTimestamp
      updatedCurrTime = if isLastCharSpace then (Just currTime) else storedCurrTime -- only update currTime at each new word
  in st -- .~ is the lens operator for setting or updating the value viewed by the lens
     & lastCharIsSpace .~ isLastCharSpace
     & startTimestamp .~ startTime
     & currTimestamp .~ updatedCurrTime
     & numTypedWords .~ (if isLastCharSpace then length wordList else length wordList - 1)
     & numIncorrect .~ if wordIsIncorrect then prevNumIncorrect + 1 else prevNumIncorrect

handleKeystrokeEvent :: T.BrickEvent EditorName e -> Bool -> T.EventM EditorName State ()
handleKeystrokeEvent e isLastCharSpace = do
  zoom editor (E.handleEditorEvent e)
  currTime <- liftIO getCurrentTime
  B.modify $ updateState isLastCharSpace currTime

handleEvent :: T.BrickEvent EditorName e -> T.EventM EditorName State ()
handleEvent (T.VtyEvent (V.EvKey (KChar 'c') [V.MCtrl])) = B.halt
handleEvent e@(T.VtyEvent (V.EvKey keyStroke _))         = 
  let noOp = [V.KEnter, V.KBS, V.KLeft, V.KRight, V.KUp, V.KDown, V.KBackTab]
  in if keyStroke `elem` noOp
        then return ()
      else handleKeystrokeEvent e (keyStroke == KChar ' ')
handleEvent e                                            = return ()

initialState :: V.Image -> State
initialState tImage = ST (E.editor EName (Just 1) "") 0 0 False Nothing Nothing tImage -- (Just 1) means limit 1 line to the editor

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr
  [ (E.editAttr,      V.withForeColor V.defAttr V.black)
  , (successAttrName, V.withForeColor V.defAttr V.green)
  , (errorAttrName,   V.withForeColor V.defAttr V.red)
  , (defaultAttrName, V.withForeColor V.defAttr V.black)
  ]

wordsPerMin :: State -> Double
wordsPerMin st = 
  let totalWords = st ^. numTypedWords
      mins = secondsToNow st / 60
      wpm = fromIntegral totalWords / mins
  -- NaN before the user begins typing, isNegativeZero on the first word due to some floating point precision stuff
  in if isNaN wpm || isNegativeZero wpm then 0.0 else wpm
    

secondsToNow :: State -> Double
secondsToNow st = 
  let defaultTime = UTCTime (ModifiedJulianDay 0) 0
      startTime = fromMaybe defaultTime (st ^. startTimestamp)
      currTime = fromMaybe defaultTime (st ^. currTimestamp)
  in realToFrac $ diffUTCTime currTime startTime

appCursor :: State -> [T.CursorLocation EditorName] -> Maybe (T.CursorLocation EditorName)
appCursor st cursorLocations
  | null (E.getEditContents $ st ^. editor) = Just $ T.CursorLocation (B.Location (0, 0)) (Just EName) True -- if the editor is empty, place the cursor at the top-left corner
  | otherwise = Nothing -- otherwise, do not place a specific cursor and rely on Brick's default behavior

app :: M.App State e EditorName 
app = 
  M.App { M.appDraw           = draw
        , M.appChooseCursor   = \_ -> B.showCursorNamed EName
        , M.appHandleEvent    = handleEvent
        , M.appStartEvent     = return ()
        , M.appAttrMap        = const appAttrMap
  }

asciiImg :: String -> IO V.Image
asciiImg path = do
  contents        <- readFile path
  let raw_lines   = lines contents
  let attr        = V.Attr V.Default V.Default V.Default V.Default
  let imgs        = map (V.string attr) raw_lines 
  return (V.vertCat imgs)

main :: IO ()
main = do
  bison   <- asciiImg "art/bison.txt" 
  st      <- M.defaultMain app (initialState bison)
  return ()
