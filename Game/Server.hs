{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game.World

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Binary
 
import Network hiding (accept, sClose)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Control.Concurrent
 
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock
 
loop sock = do
   (conn, _) <- accept sock
   forkIO $ body conn
   loop sock
  where
   body c = do sendAll c msg
               sClose c

msg :: B.ByteString
msg = B.concat . BL.toChunks $ encode $ Player 4 "Marco"