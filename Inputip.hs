{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Inputip where

import Control.Lens
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
import Data.Monoid ((<>))
import Brick.Types
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Widgets.Border as B
import Brick.Util (on)

data Name = Edit1 | Button1 | Button2
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       , _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _num :: Maybe Int
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui,buttonLayer st]
    where
        e1 = F.withFocusRing (st^.focusRing) (E.renderEditor (str . unlines)) (st^.edit1)
        ui = padTop (Pad 8) $ C.hCenterLayer $
            (str "Input IP: " <+> (hLimit 16 $ vLimit 1 e1))

buttonLayer :: St -> T.Widget Name
buttonLayer st = C.vCenterLayer  $ C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons st)

buttons st = mkButton <$> buttonData
  where buttonData = [ --(Button1, "Try Again", "again")
                       (Button1, "Back ", "back")
                     , (Button2, "Enter", "enter")
                     ]
        mkButton (name, label, attr) =
            let wasClicked = (fst <$> st^. lastReportedClick) == Just name
            in clickable name $
               withDefAttr attr $
               B.border $
               padLeftRight 1 $
               padLeftRight (if wasClicked then 2 else 3) $
               str (if wasClicked then "<" <> label <> ">" else label)



appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.MouseDown n _ _ loc) = do
    let T.Location pos = loc
    M.continue $ st & lastReportedClick .~ Just (n, loc)
appEvent st (T.MouseUp Button1 _ _) = M.halt $ st & lastReportedClick .~ Nothing
                                                  & num .~ Just 1
appEvent st (T.MouseUp Button2 _ _) = M.halt $ st & lastReportedClick .~ Nothing
                                                  & num .~ Just 2
appEvent st (T.VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev
        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
               Nothing -> return st
appEvent st _ = M.continue st

initialState :: St
initialState =
    St (F.focusRing [Edit1])
       (E.editor Edit1 Nothing "")
       []
       Nothing
       Nothing

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,   V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    , ("back",  V.white `on` V.magenta)
    , ("enter",  V.white `on` V.magenta)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

inputip :: IO (String, Maybe Int)
inputip = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v
    st <- M.customMain buildVty Nothing theApp initialState
    putStrLn "In input 1 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit1
    return (unlines $ E.getEditContents $ st^.edit1, st ^. num)
