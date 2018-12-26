{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Lens

import Test.QuickCheck

data Game = Game {
  _clears :: Int,
  _combo :: Int,
  _score :: Int
  } deriving (Show, Eq)

makeLenses ''Game

updateScore :: Game -> Game
updateScore g = 
  let increase = (g^.clears)*(100 + 50 * (g^.combo))
  in 
     g & score %~ (+ increase)
       & clears .~ 0   

newScore :: Game -> Game
newScore g@(Game c a s) =
  Game 0 a (s+ c*(100+50*a))

main = do
  putStrLn "Testing"
  quickCheck ((\(x, y, z)-> ((updateScore (Game x y z) )==(newScore (Game x y z)))):: (Int, Int, Int) -> Bool)
