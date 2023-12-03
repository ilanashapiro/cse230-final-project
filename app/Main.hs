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

drawTutor :: TutorState -> [T.Widget EditorName]
drawTutor ts = [img <=> e] 
    where
        e           = E.renderEditor (C.str . unlines) True (ts^.edit)
        img         = C.raw (tutorImg ts)

updateWordIdx :: TutorState -> TutorState
updateWordIdx ts =
  let text = E.getEditContents $ ts^.edit
      wordList = words $ concat text  -- Split text into words
  in ts & wordIdx .~ length wordList -- .~ is the lens operator for setting or updating the value viewed by the lens

handleTutorEvent :: T.BrickEvent EditorName e -> T.EventM EditorName TutorState ()
handleTutorEvent (V.VtyEvent (V.EvKey (KChar 'c') [V.MCtrl]))   = C.halt
handleTutorEvent e                                              = do
    zoom edit (E.handleEditorEvent e)
    C.modify $ updateWordIdx

initialState :: V.Image -> TutorState
initialState tImage = TS (E.editor EName Nothing "") tImage 0

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.white)
    ]

appCursor :: TutorState -> [T.CursorLocation EditorName] -> Maybe (T.CursorLocation EditorName)
appCursor ts cursorLocations
  | null (E.getEditContents $ ts ^. edit) = Just $ T.CursorLocation (C.Location (0, 0)) (Just EName) True
  | otherwise = Nothing

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
