{-# LANGUAGE TemplateHaskell #-} -- For using lenses.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Main where

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
import qualified Brick as V
import Graphics.Vty (Key(KChar))
import qualified Brick as C

data EditorName = EName deriving (Eq, Ord, Show)

data State = ST { _edit :: E.Editor String EditorName, img :: V.Image, _wordIdx :: Int }
makeLenses ''State

successAttrName :: C.AttrName
successAttrName = C.attrName "success"

errorAttrName :: C.AttrName
errorAttrName = C.attrName "error"

defaultAttrName :: C.AttrName
defaultAttrName = C.attrName "default"

referenceText :: String
referenceText = "This is an example reference text"

-- Function to compare SINGLE user input word against reference text
checkWord :: String -> Int -> Bool
checkWord inputWord wordIdx = (words referenceText) !! wordIdx == inputWord

-- Function to set word colors based on the comparison result
coloredWordsWidget :: State -> String -> T.Widget EditorName
coloredWordsWidget ts str = foldl1 (C.<+>) $ map colorizeWord $ zip [0..] (words str)
    where
        colorizeWord (idx, word) =
            let isMatch = checkWord (concat (E.getEditContents $ ts ^. edit)) idx
                attrName = if isMatch then defaultAttrName else errorAttrName
            in C.withAttr attrName $ C.str word

draw :: State -> [T.Widget EditorName]
draw ts = [img' <=> e <=> wordCount] 
    where
        e           = E.renderEditor (C.str . concat) False (ts ^. edit)
        img'        = C.raw (img ts)
        wordCount   = C.strWrap $ "Word count: " ++ show (ts ^. wordIdx)

updateWordIdx :: State -> State
updateWordIdx ts =
  let text = E.getEditContents $ ts ^. edit
      wordList = words $ concat text  -- Split text into words
  in ts & wordIdx .~ length wordList -- .~ is the lens operator for setting or updating the value viewed by the lens

handleDefaultEvent :: T.BrickEvent EditorName e -> T.EventM EditorName State ()
handleDefaultEvent e = do
        zoom edit (E.handleEditorEvent e)
        C.modify $ updateWordIdx

handleEvent :: T.BrickEvent EditorName e -> T.EventM EditorName State ()
handleEvent (V.VtyEvent (V.EvKey (KChar 'c') [V.MCtrl]))   = C.halt
handleEvent e@(V.VtyEvent (V.EvKey keyStroke _))           = 
    let noOp = [V.KEnter, V.KBS, V.KLeft, V.KRight, V.KUp, V.KDown]
    in if elem keyStroke noOp
        then return ()
      else handleDefaultEvent e
handleEvent e                                              = handleDefaultEvent e

initialState :: V.Image -> State
initialState tImage = ST (E.editor EName (Just 1) "") tImage 0 -- (Just 1) means limit 1 line to the editor

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr
    [ (E.editAttr,      V.black `on` V.white)
    , (successAttrName, V.green `on` V.white)
    , (errorAttrName,   V.red `on` V.white)
    , (defaultAttrName, V.black `on` V.white)
    ]

appCursor :: State -> [T.CursorLocation EditorName] -> Maybe (T.CursorLocation EditorName)
appCursor ts cursorLocations
    | null (E.getEditContents $ ts ^. edit) = Just $ T.CursorLocation (C.Location (0, 0)) (Just EName) True -- if the editor is empty, place the cursor at the top-left corner
    | otherwise = Nothing -- otherwise, do not place a specific cursor and rely on Brick's default behavior

app :: M.App State e EditorName 
app = 
    M.App { M.appDraw           = draw
          , M.appChooseCursor   = \_ -> C.showCursorNamed EName
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
