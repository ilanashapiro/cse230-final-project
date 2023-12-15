{-# LANGUAGE TemplateHaskell #-} -- For using lenses.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use zipWith" #-}
module Main where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
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
import Brick.Widgets.Core ( (<=>), vBox )
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)
import Control.Exception (handle)
import qualified Brick as B
import Graphics.Vty (Key(KChar))
import ImageGen (getRandomImageDocs, ImageDocs, partialImage)
import WordAnalysis (getTopThree, sortedMostMissedLetters, sortedListMissedWords,)
import qualified Brick.Widgets.List as B
import qualified Data.Text.Zipper as TZ
import qualified Data.Text as TX
import BigramPromptGen (makePrompt)
import RandomPromptGen (makeRandPrompt)
import System.Environment (getArgs)

data EditorName = EName | RefEName deriving (Eq, Ord, Show)

data State = ST {
  _editor :: E.Editor String EditorName,
  _refeditor :: E.Editor String EditorName,
  _numIncorrect :: Int,
  _numTypedWords :: Int,
  _lastCharIsSpace :: Bool,
  _startTimestamp :: Maybe UTCTime,
  _currTimestamp :: Maybe UTCTime,
  _wrongWordList :: Map.Map String Int,
  _gameover :: Bool,
  imgDocs :: ImageDocs,
  _reftxt :: String
}
makeLenses ''State

successAttrName :: A.AttrName
successAttrName = A.attrName "success"

errorAttrName :: A.AttrName
errorAttrName = A.attrName "error"

defaultAttrName :: A.AttrName
defaultAttrName = A.attrName "default"

refAttrName :: A.AttrName
refAttrName = A.attrName "refAttrName"

referenceText :: String
referenceText = "The sun dipped low on the horizon, casting a warm hue across the tranquil meadow.\
                 \ A gentle breeze whispered through the swaying grass, carrying the sweet scent of wildflowers."

coloredWordsWidget :: Bool -> String -> String -> T.Widget EditorName
coloredWordsWidget lastCharIsSpace reftxt str 
  | null (dropWhile isSpace str)  = C.str ""
  | otherwise = foldl1 (C.<+>) $ map colorizeWord $ zip [0..] inputWords
    where
      inputWords = words str
      colorizeWord (idx, word) =
          let referenceWord = words reftxt !! idx
              -- if we didn't just end the word with a space
              -- and we're not on the current word being typed
              -- and the word isn't a prefix of the reference (i.e. it could still be correct
              -- so we are still in progress)
              -- note: crashes at the end of the text
              attrName
                | not lastCharIsSpace && (idx == length inputWords - 1) && (isPrefixOf word referenceWord)
                    = defaultAttrName
                | referenceWord == word
                    = successAttrName
                | otherwise = errorAttrName
          in C.withAttr attrName $ C.str (word ++ " ")

refColorsWidget :: Int -> String -> T.Widget EditorName
refColorsWidget lastTypedWordIdx str
  | null (dropWhile isSpace str)  = C.str ""
  | otherwise =
    foldl1 (C.<+>) $ map colorizeWord $ zip [0..] inputWords
    where
      inputWords = words str
      colorizeWord (idx, word) =
              -- if we're on the current word being typed then highlight it 
          let attrName
                | idx == lastTypedWordIdx = refAttrName -- highlight current word
                | otherwise = defaultAttrName
          in C.withAttr attrName $ C.str (word ++ " ")

-- the widget for showing the game over screen once the user has finished the reference text
gameOverScreen :: B.Widget EditorName
gameOverScreen =
  vBox [ B.str "Game Over!"
          , B.str "Press 'ctrl c' to quit."
          ]

showStats :: String -> Map.Map String Int -> B.Widget EditorName
showStats acc m = vBox
  ((B.str ("Your accuracy: " ++ acc) : B.str "Your most missed words: " : map B.str (getTopThree m sortedListMissedWords)) ++
  (B.str "Your most missed letters: " : map (B.txt . TX.singleton) (getTopThree m sortedMostMissedLetters)))

draw :: State -> [T.Widget EditorName]
draw st = if gover then [intro <=> img' <=> showStats accuracy wwList <=> gameOverScreen] else [img' <=> e <=> re <=> wordCount]
  where
      e                 = E.renderEditor (coloredWordsWidget (st ^. lastCharIsSpace) referenceText . concat) True (st ^. editor)
      numTotalWords     = st ^. numTypedWords
      numIncorrectWords = st ^. numIncorrect
      wwList            = st ^. wrongWordList
      referenceText     = st ^. reftxt
      numCorrectWords   = numTotalWords - numIncorrectWords
      img'              = C.raw (partialImage (imgDocs st) numCorrectWords (length $ words referenceText))
      percentError      = if numTotalWords == 0 then 0 else (fromIntegral numCorrectWords) / (fromIntegral numTotalWords) * 100 :: Float
      spaces            = replicate 4 ' '
      wordCount         = foldl1 (C.<+>) [
                            C.withAttr defaultAttrName $ C.str ("Typed Words: " ++ show numTotalWords ++ spaces),
                            C.withAttr defaultAttrName $ C.str ("WPM: " ++ printf "%.2g" (wordsPerMin st)  ++ spaces),
                            C.withAttr defaultAttrName $ C.str ("Accuracy: " ++ printf "%.2g" percentError ++ "%" ++ spaces),
                            C.withAttr successAttrName $ C.str ("Correct: " ++ show numCorrectWords ++ spaces),
                            C.withAttr errorAttrName   $ C.str ("Errors: " ++ show numIncorrectWords)
                          ]
      text = E.getEditContents $ st ^. editor
      wordList = words $ concat text
      lastTypedWordIdx
        | null wordList = 0
        | st ^. lastCharIsSpace = length wordList
        | otherwise = length wordList - 1
      re                = E.renderEditor (refColorsWidget lastTypedWordIdx . concat) True (st ^. refeditor) -- highlight the current word to type
      gover             = st ^. gameover -- shows the game over page if we have reached the end of the reference text
      intro             = C.str "Your final image:\n"
      accuracy          = printf "%.2g" percentError ++ "%"

updateState :: Bool -> UTCTime -> State -> State
updateState isLastCharSpace currTime st =
  let text = E.getEditContents $ st ^. editor
      referenceText = st ^. reftxt
      prevNumIncorrect = st ^. numIncorrect
      wordList = words $ concat text -- Split text into words
      lastTypedWord = if null wordList then "" else last wordList
      referenceWord = words referenceText !! (length wordList - 1)
      haveNextWord = length (words referenceText) > length wordList + (previewWidth - 1)
      nextWord = if haveNextWord then words referenceText !! (length wordList + (previewWidth - 1)) else "✨"
      wordIsIncorrect = (isLastCharSpace && lastTypedWord /= referenceWord)
      storedStartTime = st ^. startTimestamp
      startTime = if storedStartTime == Nothing then Just currTime else storedStartTime
      storedCurrTime = st ^. currTimestamp
      updatedCurrTime = if isLastCharSpace then (Just currTime) else storedCurrTime -- only update currTime at each new word
      updateMap wword wwmap = case Map.lookup wword wwmap of
                              Just count -> Map.insert wword (count + 1) wwmap
                              Nothing    -> Map.insert wword 1 wwmap
      newNumTypedWords = (if isLastCharSpace then length wordList else length wordList - 1)
      newRefEditor = if isLastCharSpace && haveNextWord then E.applyEdit (TZ.insertMany (" " ++ nextWord)) (st ^. refeditor) else (st ^. refeditor)
      -- append the new word to the reference text once the user gets to it
  in st -- .~ is the lens operator for setting or updating the value viewed by the lens
     & lastCharIsSpace .~ isLastCharSpace
     & startTimestamp .~ startTime
     & currTimestamp .~ updatedCurrTime
     & numTypedWords .~ newNumTypedWords
     & wrongWordList %~ (\wlist -> if wordIsIncorrect then updateMap referenceWord wlist else wlist)
     & refeditor .~ newRefEditor
     & gameover .~ (newNumTypedWords == length (words referenceText))
     & numIncorrect .~ if wordIsIncorrect then prevNumIncorrect + 1 else prevNumIncorrect

handleKeystrokeEvent :: T.BrickEvent EditorName e -> Bool -> T.EventM EditorName State ()
handleKeystrokeEvent e isLastCharSpace = do
  zoom editor (E.handleEditorEvent e)
  currTime <- liftIO getCurrentTime
  B.modify $ updateState isLastCharSpace currTime

handleEvent :: T.BrickEvent EditorName e -> T.EventM EditorName State ()
handleEvent (T.VtyEvent (V.EvKey (KChar 'c') [V.MCtrl])) = B.halt
handleEvent e@(T.VtyEvent (V.EvKey keyStroke _))         = do
  let noOp = [V.KEnter, V.KBS, V.KLeft, V.KRight, V.KUp, V.KDown, V.KBackTab]
      currCharIsSpace = keyStroke == KChar ' '
  st <- B.get
  if keyStroke `elem` noOp || st ^. lastCharIsSpace && currCharIsSpace -- we don't want the user to have consecutive spaces
    then return ()
  else if st ^. gameover then return () else handleKeystrokeEvent e currCharIsSpace -- stops a crash if the user types an extra word 
handleEvent e                                            = return ()

previewWidth :: Int
previewWidth = 5

initialState :: ImageDocs -> String -> State
initialState iDocs reftxt = ST e re nInc nTyped lSpace startTime curTime wwlist gameover iDocs reftxt
  where
    e         = E.editor EName (Just 1) ""
    re        = E.editor RefEName (Just 2) (unwords (take previewWidth (words reftxt))) -- start out with 3 words in reference
    nInc      = 0
    nTyped    = 0
    lSpace    = False
    startTime = Nothing
    curTime   = Nothing
    wwlist    = Map.empty
    gameover  = False

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr
  [ (E.editAttr,      V.withForeColor V.defAttr V.black)
  , (successAttrName, V.withForeColor V.defAttr V.green)
  , (errorAttrName,   V.withForeColor V.defAttr V.red)
  , (refAttrName,     V.withForeColor V.defAttr (V.rgbColor 255 165 0)) -- color for the reference text
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
  args <- getArgs 
  if args == ["shakespeare"] then
    do prompt <- makePrompt "training-text/shakespeare.txt"
       iDocs  <- getRandomImageDocs "art"
       st     <- M.defaultMain app (initialState iDocs prompt)
       return ()
  else if args == ["random"] then
    do prompt <- makeRandPrompt ['a'..'z']
       iDocs  <- getRandomImageDocs "art"
       st     <- M.defaultMain app (initialState iDocs prompt)
       return ()
  else 
    do iDocs   <- getRandomImageDocs "art"
       st      <- M.defaultMain app (initialState iDocs referenceText)
       return ()