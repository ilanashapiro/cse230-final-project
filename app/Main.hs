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

data TutorState = TS { _edit :: E.Editor String EditorName, tutorImg :: V.Image, _wordIdx :: Int }
makeLenses ''TutorState


referenceText :: String
referenceText = "This is an example reference text"

-- Function to set word colors based on the comparison result
colorWords :: TutorState -> String -> [T.Widget EditorName]
colorWords ts str = map colorizeWord $ zip [0..] (words str)
    where
        colorizeWord (idx, word) =
            let isMatch = checkWord (concat (E.getEditContents $ ts ^. edit)) idx
                attr = if isMatch
                    then C.attrName "black"
                    else C.attrName "red"
            in C.withDefAttr attr $ C.str word

-- Function to compare user input against reference text
checkWord :: String -> Int -> Bool
checkWord inputText wordIdx =
    let userWords = words inputText
        referenceWords = words referenceText
    in if length referenceWords > wordIdx
        then userWords !! wordIdx == referenceWords !! wordIdx
        else False
  
drawTutor :: TutorState -> [T.Widget EditorName]
drawTutor ts = [img <=> e <=> wordCount] 
    where
        e           = E.renderEditor (C.str . unlines) False (ts ^. edit)
        img         = C.raw (tutorImg ts)
        wordCount   = C.strWrap $ "Word count: " ++ show (ts ^. wordIdx)

updateWordIdx :: TutorState -> TutorState
updateWordIdx ts =
  let text = E.getEditContents $ ts ^. edit
      wordList = words $ concat text  -- Split text into words
  in ts & wordIdx .~ length wordList -- .~ is the lens operator for setting or updating the value viewed by the lens


handleTutorEvent :: T.BrickEvent EditorName e -> T.EventM EditorName TutorState ()
handleTutorEvent (V.VtyEvent (V.EvKey (KChar 'c') [V.MCtrl]))   = C.halt
handleTutorEvent e@(V.VtyEvent (V.EvKey keyStroke _))           = 
    let noOp = [V.KEnter, V.KBS, V.KLeft, V.KRight, V.KUp, V.KDown]
    in if elem keyStroke noOp
        then return ()
      else do
        zoom edit (E.handleEditorEvent e)
        C.modify $ updateWordIdx
handleTutorEvent e                                              = do
        zoom edit (E.handleEditorEvent e)
        C.modify $ updateWordIdx


initialState :: V.Image -> TutorState
initialState tImage = TS (E.editor EName Nothing "") tImage 0


theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.white)
    , (C.attrName "black",           V.black `on` V.white)
    , (C.attrName "red",             V.red `on` V.white)
    ]

appCursor :: TutorState -> [T.CursorLocation EditorName] -> Maybe (T.CursorLocation EditorName)
appCursor ts cursorLocations
    | null (E.getEditContents $ ts ^. edit) = Just $ T.CursorLocation (C.Location (0, 0)) (Just EName) True -- if the editor is empty, place the cursor at the top-left corner
    | otherwise = Nothing -- otherwise, do not place a specific cursor and rely on Brick's default behavior

tutorApp :: M.App TutorState e EditorName 
tutorApp = 
    M.App { M.appDraw           = drawTutor
          , M.appChooseCursor   = \_ -> C.showCursorNamed EName
          , M.appHandleEvent    = handleTutorEvent
          , M.appStartEvent     = return ()
          , M.appAttrMap        = const theMap
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
    st      <- M.defaultMain tutorApp (initialState bison)
    return ()
