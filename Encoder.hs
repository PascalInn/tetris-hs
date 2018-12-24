{-# LANGUAGE OverloadedStrings #-}
module Encoder where

import qualified Data.Map.Strict as M
import Control.Lens

import Tetris

wholeBoard :: [Position]
wholeBoard = [Position x y | y <- [1..boardHeight], x <- [1..boardWidth]]

typeToCode :: Maybe TetriminoType -> Char
typeToCode (Just I) = '0'
typeToCode (Just O) = '1'
typeToCode (Just T) = '2'
typeToCode (Just S) = '3'
typeToCode (Just Z) = '4'
typeToCode (Just J) = '5'
typeToCode (Just L) = '6'
typeToCode Nothing = '7'

positionToCode :: Board -> Position -> Char
positionToCode b = typeToCode . positionToMaybe
  where
    positionToMaybe p = if M.member p b
                           then Just (b M.! p)
                           else Nothing

boardToCode :: Board -> [Char]
boardToCode b = map (positionToCode b) wholeBoard

decodeBoard :: [Char] -> Board
decodeBoard s = M.fromList [(p, (decodeType i)) | (p, i) <- zip wholeBoard s, i `elem` ['0','1','2','3','4','5','6']]
  where
    decodeType '0' = I
    decodeType '1' = O
    decodeType '2' = T
    decodeType '3' = S
    decodeType '4' = Z
    decodeType '5' = J
    decodeType '6' = L

gameToCode :: Game -> [Char]
gameToCode g = boardToCode newB
  where 
    newB = M.union (g^.stable) tetrisBoard
    tetrisBoard = M.fromList [(p, g^.movingTetris^.tType)|p <- allPosition $ g^.movingTetris]
