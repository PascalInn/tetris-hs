-- try implementation of game server with WebSockets
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server where

import qualified Network.WebSockets as WS
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Lens

import Tetris
import Encoder

data ServerState = ServerState {
  _conn :: WS.Connection,
  _selfBoard :: Board,
  _selfScore :: Int,
  _dualBoard :: Board,
  _dualScore :: Int
  }

makeLenses '' ServerState

sendGameMsg :: Game -> WS.Connection -> IO()
sendGameMsg g c = WS.sendTextData c gInfo
  where gInfo = T.pack $ gameToCode g

mainLoop :: ServerState -> WS.PendingConnection -> IO()
mainLoop s pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  talk conn s

talk :: WS.Connection -> ServerState -> IO()
talk conn s = forever $ do
  msg <- WS.receiveData conn
  pure $ updateDual msg
  WS.sendTextData conn selfInfo
  where
    selfInfo = T.pack $ (boardToCode $ s^.selfBoard) ++ (show $ s^.selfScore)
    updateDual msg = 
        s & dualBoard .~ (decodeBoard (take 200 (T.unpack msg)))
          & dualScore .~ (read (drop 200 (T.unpack msg)) :: Int)
