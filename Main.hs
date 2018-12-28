module Main where

import Tetris
import UIdef
import Game
import Cover
import Back
import Inputip
import Network.Socket as S

main :: IO ()
main = do
  num <- cover
  if num == 1
    then do
         ui <- playGame Nothing Single Nothing
         num1 <- back ui
         if num1 == 1
           then do main
           else putStrLn $ "Game Over"
    else 
      if num == 2
        then withSocketsDo $ do
               addr <- resolve Nothing "3000"
               sock <- open addr
               (conn, _) <- accept sock
               ui <- playGame Nothing Player1 (Just conn)
               num1 <- back ui
               if num1 == 1
                 then do main
                 else putStrLn $ "Game Over"
        else
          if num == 3
            then withSocketsDo $ do
                   (ip,num2) <- inputip {-ip::String -}
                   if num2 == Just 1
                     then do main
                     else do
                       addr <- resolve (Just ip) "3000"
                       sock<- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                       connect sock $ addrAddress addr
                       ui <- playGame Nothing Player2 (Just sock)
                       num1 <- back ui
                       if num1 == 1
                         then do main
                       else putStrLn $ "Game Over"
          else putStrLn $ "Game Over"

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
