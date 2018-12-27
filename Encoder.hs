{-# LANGUAGE OverloadedStrings #-}
module Encoder where

import qualified Data.Map.Strict as M
import Data.Word
import Control.Lens

import Tetris

wholeBoard :: [Position]
wholeBoard = [Position x y | y <- [1..boardHeight], x <- [1..boardWidth]]

typeToCode :: Maybe TetriminoType -> Word8
typeToCode (Just I) = toEnum 0
typeToCode (Just O) = toEnum 1
typeToCode (Just T) = toEnum 2
typeToCode (Just S) = toEnum 3
typeToCode (Just Z) = toEnum 4
typeToCode (Just J) = toEnum 5
typeToCode (Just L) = toEnum 6
typeToCode Nothing = toEnum 7

positionToCode :: Board -> Position -> Word8
positionToCode b = typeToCode . positionToMaybe
  where
    positionToMaybe p = if M.member p b
                           then Just (b M.! p)
                           else Nothing

boardToCode :: Board -> [Word8]
boardToCode b = map (positionToCode b) wholeBoard

decodeBoard :: [Word8] -> Board
decodeBoard s = M.fromList [(p, (decodeType i)) | (p, i) <- zip wholeBoard s, i `elem` [0,1,2,3,4,5,6]]
  where
    decodeType 0 = I
    decodeType 1 = O
    decodeType 2 = T
    decodeType 3 = S
    decodeType 4 = Z
    decodeType 5 = J
    decodeType 6 = L

gameToCode :: Game -> [Word8]
gameToCode g = (boardToCode newB) ++ [(toEnum $ g^.score)]
  where 
    newB = M.union (g^.stable) tetrisBoard
    tetrisBoard = M.fromList [(p, g^.movingTetris^.tType)|p <- allPosition $ g^.movingTetris]
