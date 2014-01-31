module Game.Input.Actions where

import Data.Binary

data Direction = 
	  DirNorth
	| DirEast
	| DirSouth
	| DirWest

data Action = 
	  ActionNothing
	| ActionMove Float Float -- normalized direction
	| ActionActivate Direction

instance Binary Direction where
	put DirNorth = put (0 :: Word8)
	put DirEast = put (1 :: Word8)
	put DirSouth = put (2 :: Word8)
	put DirWest = put (3 :: Word8)

	get = do
		dir <- get :: Get Word8
		case dir of
			0 -> return DirNorth
			1 -> return DirEast
			2 -> return DirSouth
			3 -> return DirWest

instance Binary Action where
	put ActionNothing = put (0 :: Word8)
	put (ActionMove x y) = do
		put (1 :: Word8)
		put x
		put y
	put (ActionActivate dir) = do
		put (2 :: Word8)
		put dir

	get = do
		t <- get :: Get Word8
		case t of 
			0 -> return ActionNothing
			1 -> do
				x <- get
				y <- get
				return $ ActionMove x y
			2 -> do
				dir <- get
				return $ ActionActivate dir
