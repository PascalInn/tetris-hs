{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Cover 
  ( cover
  ) where

import System.Exit(exitSuccess)
import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Maybe
import qualified Graphics.Vty as V
import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core


data Name = Button1 | Button2 | Button3 | Button4
          deriving (Show, Ord, Eq)

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _num :: Maybe Int
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = [buttonLayer st]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.neverShowCursor
          }

buttonLayer :: St -> Widget Name
buttonLayer st =
    C.vCenterLayer $
      C.hCenterLayer (padBottom (T.Pad 2) $ str "Tetris" ) <=>
      C.hCenterLayer (vBox $ padTopBottom 1 <$> buttons)
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (Button1, "Single ", "single")
                     , (Button2, "Player1", "player1")
                     , (Button3, "Player2", "player2")
                     , (Button4, " Quit  ", "quit")
                     ]
        mkButton (name, label, attr) =
            let wasClicked = (fst <$> st^.lastReportedClick) == Just name
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
appEvent st (T.MouseUp Button4 _ _) = M.halt st
appEvent st (T.MouseUp Button1 _ _) = M.halt $ st & lastReportedClick .~ Nothing
                                                  & num .~ Just 1
appEvent st (T.MouseUp Button2 _ _) = M.halt $ st & lastReportedClick .~ Nothing
                                                  & num .~ Just 2
appEvent st (T.MouseUp Button3 _ _) = M.halt $ st & lastReportedClick .~ Nothing
                                                  & num .~ Just 3
appEvent st (T.VtyEvent (V.EvMouseUp _ _ _)) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'Q') _)) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') _)) = M.halt st
appEvent st _ = M.continue st

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ ("single",   V.white `on` V.magenta)
    , ("player1",   V.white `on` V.magenta)
    , ("player2",   V.white `on` V.magenta)
    , ("quit",   V.white `on` V.magenta)
    ]

cover :: IO Int
cover = do 
  let buildVty = do
                   v <- V.mkVty =<< V.standardIOConfig
                   V.setMode (V.outputIface v) V.Mouse True
                   return v
  st <- M.customMain buildVty Nothing app $ St [] Nothing Nothing
  maybe exitSuccess return $ st ^.num 
