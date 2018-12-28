{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module UIdef where
import Tetris
import Control.Lens
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

data PlayingMode = Single | Player1 | Player2 deriving (Show, Eq)

data UI = UI
  { _game    :: Game         -- ^ tetris game
  , _predrop :: Maybe String -- ^ hard drop preview cell
  , _frozen  :: Bool         -- ^ freeze after hard drop before time step
  , _state   :: PlayingMode
  , _dualBoard :: Board
  , _dualScore :: Int
  , _connection :: Maybe Socket
  , _counter :: Int
  , _stopflag  :: Bool
  }

makeLenses ''UI