{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Tetris where

import Data.Map.Strict (Map)
import qualified Data.Map as M
import Control.Lens
import Data.Sequence
import qualified Data.Sequence as Seq
import System.Random (randomR, getStdRandom)
--import Data.ByteString
--import Data.Word (Word8)

import Prelude hiding (Left, Right)
import Data.Monoid

--types and instance
data TetriminoType = I | O | T | S | Z | J | L
  deriving (Show, Eq, Enum)

data Position = Position {
  _x :: Int,
  _y :: Int
  } deriving (Eq, Show)

makeLenses ''Position

--add Monoid and Ord instance of Position datatype
instance Ord Position where
  compare p1@(Position x1 y1) p2@(Position x2 y2) = compare (x1 + y1) (x2 + y2) 

instance Monoid Position where
  mempty = Position 0 0
  mappend (Position x1 y1) (Position x2 y2) = Position (x1 + x2) (y1 + y2)

data Tetrimino = Tetrimino {
  _tType :: TetriminoType,
  _center :: Position,
  _around :: [Position]
  } deriving (Show, Eq)

makeLenses ''Tetrimino

--Board datatype
--contain a Map constructed of (Position, T-Type)
--if a Position is not occupied by a Tetris, then it is not in the Map
type Board = Map Position TetriminoType

data Direction = Left | Right | Down
  deriving (Show, Eq)

data GameState = NotStarted | Playing | MultiPlayers | End
  deriving (Show, Eq)

data Game = Game {
  _gameState :: GameState,
  _movingTetris :: Tetrimino,
  _nextTetrisType :: TetriminoType,
  _clears :: Int,
  _combo :: Int,
  _inCombo :: Bool,
  _score :: Int,
  _stable :: Board
  } deriving (Show, Eq)

makeLenses ''Game

class Move o where
  move :: Direction -> o -> o
  move = moveBy 1
  moveBy :: Int -> Direction -> o -> o

instance Move Position where
  moveBy n Left (Position x y) = Position (x - n) y
  moveBy n Right (Position x y) = Position (x + n) y
  moveBy n Down (Position x y) = Position x (y - n)

instance Move Tetrimino where
  moveBy n d t =
    t & center %~ moveBy n d

--low level functions
position :: (Int, Int) -> Position
position (x, y) = Position x y

add_ :: Position -> Position -> Position
a `add_` b = Position (a^.x + b^.x) (a^.y + b^.y)

clockWise :: Position -> Position
clockWise a = Position (a^.y) (-a^.x)

counterClockWise :: Position -> Position
counterClockWise a = Position (-a^.y) (a^.x)

tetrisShape :: TetriminoType -> [Position]
tetrisShape I = map position [(-2,0),(-1,0),(1,0)]
tetrisShape O = map position [(0,-1),(1,0),(1,-1)]
tetrisShape T = map position [(-1,0),(0,-1),(1,0)]
tetrisShape S = map position [(1,0),(0,-1),(-1,-1)]
tetrisShape Z = map position [(-1,0),(0,-1),(1,-1)]
tetrisShape J = map position [(0,-1),(0,1),(-1,-1)]
tetrisShape L = map position [(0,-1),(0,1),(1,-1)]

--basic rotations defined by Nintendo
rotateTetrimino :: Tetrimino -> Tetrimino
rotateTetrimino t
  |(t^.tType) == O = t
  |(t^.tType) == I && (Position 0 (-1) `elem` (t^.around)) = Tetrimino (t^.tType) (t^.center) (map clockWise (t^.around))
  |(t^.tType) == S && (Position 1 (-1)) `elem` (t^.around) = Tetrimino (t^.tType) (t^.center) (map clockWise (t^.around))
  |(t^.tType) == Z && (Position 1 1) `elem` (t^.around) = Tetrimino (t^.tType) (t^.center) (map clockWise (t^.around))
  |otherwise = Tetrimino (t^.tType) (t^.center) (map counterClockWise (t^.around))

allPosition :: Tetrimino -> [Position]
allPosition t = t^.center :
                (map (add_ (t^.center)) (t^.around))

initTetri :: TetriminoType -> Tetrimino
initTetri a =
  Tetrimino a startPosition (tetrisShape a)

startPosition :: Position
startPosition = Position 6 22

boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

randomType :: IO TetriminoType
randomType = do
  randomPosition <- getStdRandom (randomR (0, 6))
  let (_, right) = Seq.splitAt randomPosition (Seq.fromList [I,O,T,S,Z,J,L]) 
      (y Seq.:< ys) = Seq.viewl right
  -- y <- Prelude.lookup randomPosition [(0,I),(1,O),(2,T),(3,S),(4,Z),(5,J),(6,L)]
  -- fromMaybe y
  return y

--functions on game or board
initGame :: IO Game
initGame = do
  firstT <- randomType
  nextT <- randomType
  pure $ 
    Game NotStarted (initTetri firstT) nextT 0 0 False 0 mempty

safeMoving :: Board -> Tetrimino -> Bool
safeMoving b t = all allSafe ( allPosition t)
  where 
    allSafe = (&&) <$> (`M.notMember` b) <*> withinBoard
    withinBoard (Position x y) = (x >= 1) && (x <= boardWidth) && (y >= 1)

--replace tetris block with safeMoving
--tetrisBlocked :: Board -> Tetrimino -> Bool
--tetrisBlocked b = any cellBlocked . allPosition
--  where cellBlocked = (||) <$> atBottom <*> (`M.member` b)
--        atBottom (Position x y) = y == 1

gameBlocked :: Game -> Bool
gameBlocked g = not (safeMoving (g^.stable) (move Down (g^.movingTetris)))
--  tetrisBlocked (g^.stable) (g^.movingTetris)

gameOver :: Game -> Bool
gameOver g = (gameBlocked g) 
             && (g ^. movingTetris ^. center == startPosition)

clearFullRows :: Game -> Game
clearFullRows g = g & stable %~ clearBoard
                    & clears .~ count
                    & inCombo .~ setBool
                    & combo %~ setCombo
  where
    clearBoard = M.mapKeys moveAbove . M.filterWithKey notClearRows
    count = Prelude.length clearRows
    isFull r = boardWidth == (Prelude.length (M.filterWithKey (inRow r) (g^.stable)))
    inRow r (Position x y) _ = r == y
    clearRows = Prelude.filter isFull [1..boardHeight]
    notClearRows (Position x y) _ = notElem y clearRows
    moveAbove (Position x y)= 
      let down = Prelude.length (Prelude.filter (<y) clearRows)
      in Position x (y-down)
    setBool = count > 0
    setCombo a = if setBool
                    then a + 1
                    else 0

updateScore :: Game -> Game
updateScore g = 
  let increase = (g^.clears)*(100 + 50 * (g^.combo))
  in 
     g & score %~ (+ increase)
       & clears .~ 0          

endMovingTetris :: Game -> Game
endMovingTetris g = g & stable %~ M.union newStable
  where
    newStable = M.fromList [(p, g^.movingTetris^.tType)|p <- allPosition (g^.movingTetris)]

newTetris :: Game -> IO Game
newTetris g = do
  a <- randomType
  pure $
    g & movingTetris .~ initTetri (g^.nextTetrisType)
      & nextTetrisType .~ a

gameMove :: Direction -> Game -> Game
gameMove d g = g & movingTetris %~ moveSafely
  where
    moveSafely t = if safeMoving (g^.stable) (move d t)
                      then move d t
                      else t

autoStep :: Game -> Game
autoStep g = gameMove Down g

twoTimesStep :: Game -> Game
twoTimesStep g = gameMove Down (gameMove Down g)

timeStep :: Game -> IO Game
timeStep g = 
  if gameBlocked g
    then newTetris . updateScore . clearFullRows . endMovingTetris $ g
    else pure . autoStep $ g

--maybe can be reduced, ugly code
rotateInGame :: Board -> Tetrimino -> Tetrimino
rotateInGame b t = if safeMoving b (rotateTetrimino t)
                      then rotateTetrimino t
                      else 
                        if additionalRule b t
                           then t
                           else
                             if safeMoving b (move Left (rotateTetrimino t))
                                then move Left (rotateTetrimino t)
                                else
                                  if safeMoving b (move Right (rotateTetrimino t))
                                     then move Right (rotateTetrimino t)
                                     else
                                       if (t^.tType == I) && (safeMoving b (moveBy 2 Right (rotateTetrimino t)))
                                          then moveBy 2 Right (rotateTetrimino t)
                                          else t

  where
    additionalRule b t = 
      if t^.tType `elem` [L,J,T]
         then any (`M.member` b) (map (add_ (t^.center)) [(Position 0 1),(Position 0 (-1))])
         else False                                

gameRotate :: Game -> Game
gameRotate g = g & movingTetris %~ (rotateInGame (g^.stable))

fastDrop :: Game -> Game
fastDrop g = g & movingTetris .~ fastDropT g

fastDropT :: Game -> Tetrimino
fastDropT g = moveBy n Down $ g^.movingTetris
  where
    n = minimum diff
    diff = [y2 - y1 - 1 | (Position x1 y1) <- (M.keys (g^.stable)), 
                          (Position x2 y2) <- (allPosition (g^.movingTetris)), 
                          x1 == x2, y1 < y2]
