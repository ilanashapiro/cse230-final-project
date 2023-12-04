{-# LANGUAGE TemplateHaskell #-} -- For using lenses.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Main where

import Data.List (isPrefixOf)

import Lens.Micro
import Lens.Micro.Mtl (zoom)
import Lens.Micro.TH ( makeLenses )
import qualified Graphics.Vty as V

--import Lens.Micro.Mtl
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

data State = ST { _editor :: E.Editor String EditorName, img :: V.Image, _wordIdx :: Int, _numIncorrect :: Int, _lastCharIsSpace :: Bool }
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

-- Function to set word colors based on the comparison result
coloredWordsWidget :: Bool -> String -> T.Widget EditorName
coloredWordsWidget _ "" = C.str ""
coloredWordsWidget lastCharIsSpace str = foldl1 (C.<+>) $ map colorizeWord $ zip [0..] inputWords
    where
        inputWords = words str
        colorizeWord (idx, word) =
            let referenceWord = words referenceText !! idx
                attrName = 
                    if not lastCharIsSpace && (idx == length inputWords - 1) && (isPrefixOf word referenceWord)
                        then defaultAttrName
                    else if referenceWord == word 
                        then successAttrName 
                    else errorAttrName
            in C.withAttr attrName $ C.str (word ++ " ")

draw :: State -> [T.Widget EditorName]
draw ts = [img' <=> e <=> wordCount] 
    where
        e           = E.renderEditor ((coloredWordsWidget (ts ^. lastCharIsSpace)) . concat) True (ts ^. editor)
        img'        = C.raw (img ts)
        wordCount   = C.strWrap $ "Word count: " ++ show (ts ^. wordIdx)

updateWordIdx :: State -> State
updateWordIdx ts =
  let text = E.getEditContents $ ts ^. editor
      wordList = words $ concat text  -- Split text into words
  in ts & wordIdx .~ length wordList -- .~ is the lens operator for setting or updating the value viewed by the lens

handleDefaultEvent :: T.BrickEvent EditorName e -> Bool -> T.EventM EditorName State ()
handleDefaultEvent e isLastCharSpace = do
    zoom editor (E.handleEditorEvent e)
    B.modify (\ts -> ts & lastCharIsSpace .~ isLastCharSpace)
    B.modify updateWordIdx

handleEvent :: T.BrickEvent EditorName e -> T.EventM EditorName State ()
handleEvent (T.VtyEvent (V.EvKey (KChar 'c') [V.MCtrl])) = B.halt
handleEvent e@(T.VtyEvent (V.EvKey (KChar ' ') _))       = do
    handleDefaultEvent e True
handleEvent e@(T.VtyEvent (V.EvKey keyStroke _))         = 
    let noOp = [V.KEnter, V.KBS, V.KLeft, V.KRight, V.KUp, V.KDown]
    in if elem keyStroke noOp
         then return ()
       else handleDefaultEvent e False
handleEvent e                                            = handleDefaultEvent e False

initialState :: V.Image -> State
initialState tImage = ST (E.editor EName (Just 1) "") tImage 0 0 False -- (Just 1) means limit 1 line to the editor

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr
    [ (E.editAttr,      V.withForeColor V.defAttr V.black)
    , (successAttrName, V.withForeColor V.defAttr V.green)
    , (errorAttrName,   V.withForeColor V.defAttr V.red)
    , (defaultAttrName, V.withForeColor V.defAttr V.black)
    ]

appCursor :: State -> [T.CursorLocation EditorName] -> Maybe (T.CursorLocation EditorName)
appCursor ts cursorLocations
    | null (E.getEditContents $ ts ^. editor) = Just $ T.CursorLocation (B.Location (0, 0)) (Just EName) True -- if the editor is empty, place the cursor at the top-left corner
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
