{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game.World

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Binary
 
import Network.Simple.TCP
import Control.Concurrent
 
main = do
	connect "127.0.0.1" "5002" $ \(sock, addr) -> do
		maybeStr <- recv sock 256
		case maybeStr of
			Just str -> do
				print $ (decode (BL.fromChunks ([str])) :: Player)
			Nothing -> return ()

		return ()

