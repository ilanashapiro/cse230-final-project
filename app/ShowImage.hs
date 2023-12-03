
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

import ImageGen (ImageDocs, partialImage, getRandomImageDocs)

data ImageName = IName deriving (Eq, Ord, Show)
data ImageState = IS {cur :: Int, stop :: Int, imgDocs :: ImageDocs}

drawTutor :: ImageState -> [T.Widget ImageName]
drawTutor is = [img]
    where
        img = C.raw $ partialImage (imgDocs is) (cur is) (stop is)

handleTutorEvent :: T.BrickEvent ImageName e -> T.EventM ImageName ImageState()
handleTutorEvent (V.VtyEvent (V.EvKey (KChar 'c') [V.MCtrl]))   = C.halt
handleTutorEvent (V.VtyEvent (V.EvKey (KChar 'g') []))          = C.halt
handleTutorEvent (V.VtyEvent (V.EvKey (KChar 'e') []))          = C.halt

initialState :: ImageDocs -> ImageState 
initialState imgDocs = IS 0 20 imgDocs

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (E.editFocusedAttr,            V.black `on` V.yellow)
    ]

appCursor :: ImageState -> [T.CursorLocation ImageName] -> Maybe (T.CursorLocation ImageName)
appCursor _ _ = Nothing 

tutorApp :: M.App ImageState e ImageName 
tutorApp = 
    M.App { M.appDraw           = drawTutor
          , M.appChooseCursor   = appCursor
          , M.appHandleEvent    = handleTutorEvent
          , M.appStartEvent     = return ()
          , M.appAttrMap        = const theMap
    }

main :: IO ()
main = do
    iDocs   <- getRandomImageDocs "./art"
    st      <- M.defaultMain tutorApp (initialState iDocs)
    return ()
