{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Game where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (Left, Right)
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T

import UIdef
import Encoder
import Tetris

import Brick hiding (Down)
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Map (Map)
import qualified Data.Map as M
import Control.Lens
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as S

-- | Ticks mark passing of time
data Tick = Tick | Tok

-- | Named resources
type Name = ()

data CellLocation = InGrid | InNextShape
data TVisual = Normal | FastDrop

-- App definition and execution
app :: App UI Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

playGame :: Maybe String -> PlayingMode -> Maybe Socket -> IO UI
playGame mp pm conn = do
  let delay = levelToDelay
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  forkIO $ forever $ do
    writeBChan chan Tok
    threadDelay 49999
  initialGame <- initGame 
  let initialUI = UI initialGame mp False pm mempty 0 conn 0 False
  customMain (V.mkVty V.defaultConfig) (Just chan) app initialUI 

levelToDelay :: Int
levelToDelay  = floor $ 500000 

-- Handling events

handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)
handleEvent ui (AppEvent Tick)
  | (cmp (ui^.state) Single) && (ui ^. stopflag == True) = halt ui
  | (ui ^. counter > 606) && not(cmp (ui^.state) Single) = halt ui
  | otherwise                                            = handleTick ui
handleEvent ui (AppEvent Tok)                            = handleTok ui
handleEvent ui (VtyEvent (V.EvKey V.KRight []))          = frozenGuard (gameMove Right) ui
handleEvent ui (VtyEvent (V.EvKey V.KLeft []))           = frozenGuard (gameMove Left) ui
handleEvent ui (VtyEvent (V.EvKey V.KDown []))           = frozenGuard (gameMove Down) ui
handleEvent ui (VtyEvent (V.EvKey V.KUp []))             = frozenGuard gameRotate ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') []))     = continue $ ui & game %~ fastDrop
                                                                     & frozen .~ True
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'r') []))     = restart ui
handleEvent ui (VtyEvent (V.EvKey V.KEsc []))            = halt ui
handleEvent ui _                                         = continue ui

cmp:: PlayingMode -> PlayingMode -> Bool
cmp Single  Single = True
cmp Player1 Player1 = True
cmp Player2 Player2 = True
cmp Player1 Player2 = True
cmp Player2 Player1 = True
cmp Single  Player1 = False
cmp Single  Player2 = False
cmp Player1 Single  = False
cmp Player2 Single  = False

-- | If frozen, return same UI, else execute game op
frozenGuard :: (Game -> Game) -> UI -> EventM Name (Next UI)
frozenGuard op ui = continue
  $ if ui ^. frozen
       then ui
       else ui & game %~ op

-- | Handles time steps, does nothing if game is over
handleTick :: UI -> EventM Name (Next UI)
handleTick ui =
  if gameOver g
     then continue $ ui & stopflag .~ True
     else 
       if ui ^. counter < 6
         then do continue $ ui & counter .~ (ui ^. counter + 1)
       else do
            g' <- liftIO (timeStep g)
            continue $ ui & game .~ g'
                          & frozen .~ False
                          & counter .~ (ui ^. counter + 1)
  where g = ui ^. game

handleTok :: UI -> EventM Name (Next UI)
handleTok ui = 
  if ui ^. state == Single
     then continue $ ui
     else do
            msg <- liftIO $ talk (ui ^. connection) ui
            continue $ ui & dualBoard .~ (decodeBoard (take 200 (S.unpack msg)))
                          & dualScore .~ fromEnum (head $ drop 200 (S.unpack msg))

talk :: Maybe Socket -> UI -> IO S.ByteString
talk (Just sock) ui = 
  if ui ^. state == Player1
    then do
      msg <- recv sock 4096
      sendAll sock selfInfo
      return msg
    else do
      sendAll sock selfInfo
      msg <- recv sock 4096
      return msg
  where
    selfInfo = S.pack $ gameToCode $ ui ^. game

-- | Restart game at the same level
restart :: UI -> EventM Name (Next UI)
restart ui = do
  g <- liftIO $ initGame
  continue $ ui & game .~ g
                & frozen .~ False

-- Drawing
drawUI :: UI -> [Widget Name]
drawUI ui
  | ui ^. state == Single
  =[ C.vCenter $ vLimit 22 $ hBox [ padLeft Max $ drawGrid ui
                                 , padRight Max $ padLeft (Pad 3) $ drawInfo ui
                                 ]
  ]
  | otherwise
  =[ C.vCenter $ vLimit 22 $ hBox [ padLeft Max $ drawGrid ui
                                 , drawInfo ui
                                 , padRight Max $ drawenemyGrid ui
                                 ]
  ] 

drawGrid :: UI -> Widget Name
drawGrid ui = hLimit 22
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str name)
  $ vBox rows
  where 
    name 
      | ui ^. state == Single  = "Tetris"
      | ui ^. state == Player1 = "Player1"
      | ui ^. state == Player2 = "Player2"
    rows = [foldr (<+>) emptyWidget $ M.filterWithKey (inRow r) gmap
             | r <- [boardHeight,boardHeight-1..1]
           ]
    inRow r (Position _ y) _ = r == y
    gmap = mconcat [brdMap, cBlkMap, hrdMap, emptyCellMap]
    brdMap = draw Normal . Just <$> g ^. stable
    hrdMap = blkMap (fastDropT g) FastDrop
    cBlkMap = blkMap (g ^. movingTetris) Normal
    blkMap b v = M.fromList . map (, draw v . Just $ b ^. tType) $ allPosition b
    draw = drawMCell (ui ^. predrop) InGrid
    g = ui ^. game

drawenemyGrid :: UI -> Widget Name
drawenemyGrid ui = hLimit 22
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str name)
  $ vBox rows
  where 
    name 
      | ui ^. state == Player1 = "Player2"
      | ui ^. state == Player2 = "Player1"
    rows = [foldr (<+>) emptyWidget $ M.filterWithKey (inRow r) gmap
             | r <- [boardHeight,boardHeight-1..1]
           ]
    inRow r (Position _ y) _ = r == y
    gmap = mconcat [brdMap, emptyCellMap]
    brdMap = draw Normal . Just <$> ui ^. dualBoard
    draw = drawMCell (ui ^. predrop) InGrid

emptyCellMap :: Map Position (Widget Name)
emptyCellMap = M.fromList cws
  where
    cws = [((Position x y), ew) | x <- [1..boardWidth], y <- [1..boardHeight]]
    ew = drawMCell Nothing InGrid Normal Nothing

drawMCell :: Maybe String -> CellLocation -> TVisual -> Maybe TetriminoType -> Widget Name
drawMCell _ InGrid _ Nothing      = withAttr emptyAttr cw
drawMCell _ InNextShape _ Nothing = withAttr emptyAttr ecw
drawMCell mp _ v (Just t)         = drawCell mp t v

drawCell :: Maybe String -> TetriminoType -> TVisual ->  Widget Name
drawCell _ t Normal          = withAttr (tToAttr t) cw
drawCell Nothing t FastDrop  = withAttr (tToAttrH t) hcw
drawCell (Just p) t FastDrop = withAttr (tToAttrH t) (str p)

tToAttr I = iAttr
tToAttr O = oAttr
tToAttr T = tAttr
tToAttr S = sAttr
tToAttr Z = zAttr
tToAttr J = jAttr
tToAttr L = lAttr

tToAttrH I = ihAttr
tToAttrH O = ohAttr
tToAttrH T = thAttr
tToAttrH S = shAttr
tToAttrH Z = zhAttr
tToAttrH J = jhAttr
tToAttrH L = lhAttr

cw :: Widget Name {-屏幕中的空点-}
cw = str " ."

ecw :: Widget Name{--}
ecw = str "  "

hcw :: Widget Name{-要下落的地方-}
hcw = str "[]"

drawStat2 :: String -> Int -> String -> Widget Name
drawStat2 s1 n s2= padLeftRight 1
  $ str s1 <+> (padLeft Max $ str $ show n) <+> (str s2)

drawStat :: String -> Int -> Widget Name
drawStat s n = padLeftRight 1
  $ str s <+> (padLeft Max $ str $ show n)

drawLeaderBoard :: Game -> Widget Name
drawLeaderBoard g = emptyWidget

drawInfo :: UI -> Widget Name
drawInfo ui = hLimit 18 -- size of next piece box
  $ vBox [ drawNextShape (ui ^. game ^. nextTetrisType)
         , padTop (Pad 1) $ drawScore ui
         , padTop (Pad 1) $ drawHelp
         , padTop (Pad 1) $ drawGameOver (ui ^. game)
         ]

drawScore :: UI -> Widget Name
drawScore ui 
  | (ui^.state) == Single && (ui^.counter) <=6     
      = hLimit 18
        $ padTopBottom 0
        $ withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Score")
        $ vBox [ drawStat "score" $ (ui ^. game ^. score)
               , padTop (Pad 1) $ drawStat2 "count down " (display (ui^.counter)) "s"
               , drawLeaderBoard (ui ^. game)
               ] 
  | (ui^.state) == Single && (ui^.counter) > 6     
      = hLimit 18
        $ padTopBottom 0
        $ withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Score")
        $ vBox [ drawStat "score" $ (ui ^. game ^. score)
               , padTop (Pad 1) $ str " "
               , drawLeaderBoard (ui ^. game)
               ]                          
  | (ui^.counter) <= 6                     
      = hLimit 18
        $ padTopBottom 0
        $ withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Score")
        $ vBox [ drawStat "my   " $ (ui ^. game ^. score)
               , padTop (Pad 0) $ drawStat "enemy" $ (ui ^. dualScore)
               , padTop (Pad 0) $ drawStat2 "count down " (display (ui^.counter)) "s"
               , drawLeaderBoard (ui ^. game)
               ]   
  | otherwise                    
      = hLimit 18
        $ padTopBottom 0
        $ withBorderStyle BS.unicodeBold
        $ B.borderWithLabel (str "Score")
        $ vBox [ drawStat "my   " $ (ui ^. game ^. score)
               , padTop (Pad 0) $ drawStat "enemy" $ (ui ^. dualScore)
               , padTop (Pad 0) $ drawStat2 "time " (display (ui^.counter)) "s"
               , drawLeaderBoard (ui ^. game)
               ]  

display :: Int -> Int
display time 
  | time <= 6  = 3 - (time `div` 2)
  | otherwise  = 300 - (time `div` 2)
drawNextShape :: TetriminoType -> Widget Name
drawNextShape t = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Next")
  $ padTopBottom 1 $ padLeftRight 4
  $ vLimit 16
  $ vBox $ mkRow <$> [1,0,-1]
  where
    mkRow y = hBox $ drawMCell Nothing InNextShape Normal . cellAt . (`Position` y) <$> [-2..2]
    cellAt (Position x y) = if (Position x y) `elem` cs then Just t else Nothing
    blk = Tetrimino t (Position 0 0) (tetrisShape t)
    cs = allPosition blk

drawHelp :: Widget Name
drawHelp = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Help")
  $ padTopBottom 0
  $ vBox $ map (uncurry drawKeyInfo)
  $ [ ("Left", "←")
    , ("Right", "→")
    , ("Down", "↓")
    , ("Rotate", "↑")
    , ("Drop", "space")
    , ("Restart", "r")
    , ("Quit", "Esc")
    ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  (padRight Max $ padLeft (Pad 1) $ str action)
  <+> (padLeft Max $ padRight (Pad 1) $ str keys)

drawGameOver :: Game -> Widget Name
drawGameOver g = if (gameOver g)
                    then padLeftRight 4 $ withAttr gameOverAttr $ str "GAME OVER"
                    else emptyWidget

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (iAttr, tToColor I `on` tToColor I)
  , (oAttr, tToColor O `on` tToColor O)
  , (tAttr, tToColor T `on` tToColor T)
  , (sAttr, tToColor S `on` tToColor S)
  , (zAttr, tToColor Z `on` tToColor Z)
  , (jAttr, tToColor J `on` tToColor J)
  , (lAttr, tToColor L `on` tToColor L)
  , (ihAttr, fg $ tToColor I)
  , (ohAttr, fg $ tToColor O)
  , (thAttr, fg $ tToColor T)
  , (shAttr, fg $ tToColor S)
  , (zhAttr, fg $ tToColor Z)
  , (jhAttr, fg $ tToColor J)
  , (lhAttr, fg $ tToColor L)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

tToColor :: TetriminoType -> V.Color
tToColor I = V.cyan
tToColor O = V.yellow
tToColor T = V.magenta
tToColor S = V.green
tToColor Z = V.red
tToColor J = V.blue
tToColor L = V.white

iAttr, oAttr, tAttr, sAttr, zAttr, jAttr, lAttr :: AttrName
iAttr = "I"
oAttr = "O"
tAttr = "T"
sAttr = "S"
zAttr = "Z"
jAttr = "J"
lAttr = "L"

ihAttr, ohAttr, thAttr, shAttr, zhAttr, jhAttr, lhAttr :: AttrName
ihAttr = "Ih"
ohAttr = "Oh"
thAttr = "Th"
shAttr = "Sh"
zhAttr = "Zh"
jhAttr = "Jh"
lhAttr = "Lh"

emptyAttr :: AttrName
emptyAttr = "empty"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"
