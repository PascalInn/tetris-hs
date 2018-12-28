{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Back where


import Tetris
import UIdef

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
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
import Brick.Types 
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core

--data PlayingMode = Single | Player1 | Player2 deriving (Show, Eq)

data Name = Button1 | Button2
          deriving (Show, Ord, Eq)
{-
data UI = UI
  { _game    :: Game         -- ^ tetris game
  , _predrop :: Maybe String -- ^ hard drop preview cell
  , _frozen  :: Bool         -- ^ freeze after hard drop before time step
  , _state   :: PlayingMode
  , _dualBoard :: Board
  , _dualScore :: Int
  , _connection :: Maybe Socket
  }
-}

data St =
    St { _clicked :: [T.Extent Name]
       , _lastReportedClick :: Maybe (Name, T.Location)
       , _gamemsg :: UI
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
buttonLayer st 
  | (st ^. gamemsg ^. state) == Single
    = C.vCenterLayer $
      C.hCenterLayer (padBottom (T.Pad 1) $ str "Game Over" ) <=>
      C.hCenterLayer (padBottom (T.Pad 1) $ hLimit 20 $ drawStat "final score " $ (st ^. gamemsg ^. game ^. score) ) <=>
      C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons st)
  | otherwise
    = C.vCenterLayer $
      C.hCenterLayer (padBottom (T.Pad 1) $ str "Game Over" ) <=>
      C.hCenterLayer (padBottom (T.Pad 1) $ hLimit 20 $ drawStat "final score " $ (st ^. gamemsg ^. game ^. score) ) <=>
      C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons st)

showstring :: St -> Widget Name
showstring st 
  | (st ^. gamemsg ^. game ^. score) > (st ^. gamemsg ^. dualScore) = str "You Win!"
  | otherwise                                                       = str "You Lose"


buttons st = mkButton <$> buttonData
  where buttonData = [ --(Button1, "Try Again", "again")
                       (Button1, "Home", "home")
                     , (Button2, "Quit", "quit")
                     ]
        mkButton (name, label, attr) =
            let wasClicked = (fst <$> st^. lastReportedClick) == Just name
            in clickable name $
               withDefAttr attr $
               B.border $
               padLeftRight 1 $
               padLeftRight (if wasClicked then 2 else 3) $
               str (if wasClicked then "<" <> label <> ">" else label)

drawStat :: String -> Int -> Widget Name
drawStat s n = padLeftRight 1
  $ str s <+> (padLeft Max $ str $ show n)

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.MouseDown n _ _ loc) = do
    let T.Location pos = loc
    M.continue $ st & lastReportedClick .~ Just (n, loc)
appEvent st (T.MouseUp Button1 _ _) = M.halt $ st & lastReportedClick .~ Nothing
                                                  & num .~ Just 1
appEvent st (T.MouseUp Button2 _ _) = M.halt $ st & lastReportedClick .~ Nothing
                                                  & num .~ Just 2
appEvent st (T.VtyEvent (V.EvMouseUp _ _ _)) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'Q') _)) = M.halt st
appEvent st (T.VtyEvent (V.EvKey (V.KChar 'q') _)) = M.halt st
appEvent st _ = M.continue st

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ --("again",   V.white `on` V.magenta)
      ("home",   V.white `on` V.magenta)
    , ("quit",   V.white `on` V.magenta)
    ]

back :: UI -> IO Int
back ui = do 
  let buildVty = do
                   v <- V.mkVty =<< V.standardIOConfig
                   V.setMode (V.outputIface v) V.Mouse True
                   return v
  st <- M.customMain buildVty Nothing app  $ St [] Nothing ui Nothing
  maybe exitSuccess return $ st ^.num 
