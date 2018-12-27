module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Exit (exitSuccess)
import System.IO (readFile, writeFile)
import Text.Read (readMaybe)

import Tetris
import Game
import Cover

import Options.Applicative
import qualified System.Directory as D
import System.FilePath ((</>))
import qualified System.FilePath as F
import Network.Socket as S

data Opts = Opts
  { hardDrop :: Maybe HardDropOpt
  , level    :: Maybe Int
  , score    :: Bool
--  , gamemode :: Maybe Int
  }

data HardDropOpt = AsciiOnly | CustomChars String

--data PlayingMode = Single | Player1 | Player2 deriving (Show, Eq)

opts :: Parser Opts
opts = Opts
  <$> optional hardDropOpt
  <*> optional (option auto
    (  long "level"
    <> short 'l'
    <> metavar "LEVEL"
    <> help "Specify level (unspecified results in prompt)" ))
  <*> switch
    (  long "high-score"
    <> help "Print high score and exit" )
{-  <*> optional (option auto
    (  long "game-mode"
    <> short 'g'
    <> showDefault
    <> value 0
    <> metavar "GAME-MODE"
    <> help "Choose game mode"))-}

hardDropOpt :: Parser HardDropOpt
hardDropOpt = asciiOpt <|> custOpt
  where
    asciiOpt = flag' AsciiOnly
      (  long "ascii-only"
      <> short 'a'
      <> help "Use '[]' as hard drop preview cell instead of '◤◢'" )
    custOpt = CustomChars <$> option twoChar
      (  long "preview-chars"
      <> short 'p'
      <> metavar "CHARS"
      <> help "Custom two character preview cell" )

fullopts :: ParserInfo Opts
fullopts = info (helper <*> opts)
  (  fullDesc
  <> header "tetris - the iconic game right in your terminal" )

twoChar :: ReadM String
twoChar = do
  cs <- str
  if length cs /= 2
     then readerError "Preview must be two characters long"
     else return cs

hdOptStr :: HardDropOpt -> String
hdOptStr AsciiOnly = "[]"
hdOptStr (CustomChars s) = s

defaultAddr :: String
defaultAddr = "10.19.74.166"

main :: IO ()
main = do
  (Opts hd ml hs) <- execParser fullopts           -- get CLI opts/args
  let mp = hdOptStr <$> hd                         -- determine hard drop preview cell
  when hs (getHighScore >>= printM >> exitSuccess) -- show high score and exit
  num <- fromMaybe cover (return <$> ml)
  if num == 1
    then do
         g <- playGame mp Single Nothing
         handleEndGame (_score g)
    else 
      if num == 2
        then withSocketsDo $ do
               addr <- resolve Nothing "3000"
               sock <- open addr
               (conn, _) <- accept sock
               g <- playGame mp Player1 (Just conn)
               handleEndGame (_score g) 
        else withSocketsDo $ do
               addr <- resolve (Just defaultAddr) "3000"
               sock<- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
               connect sock $ addrAddress addr
               g <- playGame mp Player2 (Just sock)
               handleEndGame (_score g)

resolve Nothing port = do 
  let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
  addr:_<-getAddrInfo (Just hints) Nothing (Just port)
  return addr
resolve (Just host) port = do
  let hints = defaultHints{addrSocketType = Stream}
  addr:_<-getAddrInfo (Just hints) (Just host) (Just port)
  return addr  

open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr)(addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 3000  
  return sock

handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> newHighScore
    Just hs -> if s <= hs then justShowScore else newHighScore
  where
    justShowScore = putStrLn $ "Your final score: " ++ show s
    newHighScore = do
      putStrLn $ "Congrats! You just got the new highest score: " ++ show s
      setHighScore s

printM :: Show a => Maybe a -> IO ()
printM Nothing  = putStrLn "None"
printM (Just s) = print s

getHighScore :: IO (Maybe Int)
getHighScore = do
  lb <- getLeaderboardFile
  exists <- D.doesFileExist lb
  if exists
     then readMaybe <$> readFile lb
     else return Nothing

setHighScore :: Int -> IO ()
setHighScore s = do
  lb <- getLeaderboardFile
  writeFile lb (show s)

getLeaderboardFile :: IO FilePath
getLeaderboardFile = do
  xdg <- D.getXdgDirectory D.XdgData "tetris"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "leaderboard")
