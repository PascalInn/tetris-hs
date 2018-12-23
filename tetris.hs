{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Tetris where

import Data.Map.Strict (Map)
import qualified Data.Map as M
import Control.Lens
import Data.Sequence
import qualified Data.Sequence as Seq
import System.Random (randomR, getStdRandom)

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
tetrisShape J = map position [(0,-1),(0,-2),(-1,-2)]
tetrisShape L = map position [(0,-1),(0,-2),(1,-2)]

rotateTetrimino :: Tetrimino -> Tetrimino
rotateTetrimino t
  |(t^.tType) == O = t
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
  return y

--functions on game or board
initGame :: IO Game
initGame = do
  firstT <- randomType
  nextT <- randomType
  pure $ 
    Game NotStarted (initTetri firstT) nextT 0 0 False 0 mempty

tetrisBlocked :: Board -> Tetrimino -> Bool
tetrisBlocked board = any cellBlocked . allPosition
  where cellBlocked = (||) <$> atBottom <*> (`M.member` board) . (move Down)
        atBottom (Position x y) = y == 1

gameBlocked :: Game -> Bool
gameBlocked g = tetrisBloced (g^.stable) (g^.movingTetris)

gameOver :: Game -> Bool
gameOver g = (gameBlocked g) 
             && (g ^. movingTetris ^. center == startPosition)

clearFullRows :: Game -> Game
clearFullRows g = g & stable %~ clearBoard
                    & clears %~ (addRowClears count)
  where
    clearBoard = M.mapKeys 


