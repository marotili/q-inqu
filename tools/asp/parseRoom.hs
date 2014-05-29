{-# LANGUAGE TemplateHaskell #-}
import Text.Regex.Posix
import Control.Lens

data TileAnswer = TileAnswer
	{ _taTiles :: [Tile]
	} deriving (Show)

data Tile = Tile
	{ _tileId :: Int -- not needed
	, _tilePolyId :: Int -- not needed
	, _tileWallId :: String
	, _tileX :: Int
	, _tileY :: Int
	} deriving (Show)

makeLenses ''TileAnswer
makeLenses ''Tile

newTile [_, id, poly, wall, x, y] = Tile {}
	& tileId .~ 0
	& tilePolyId .~ 0
	& tileWallId .~ wall
	& tileX .~ (read x)
	& tileY .~ (read y)
newTile _ = error "unknown format"

newTileAnswer = TileAnswer

main = do
	fileContent <- readFile "tools/asp/room_output"
	let test = (fileContent =~ "^setTile.*\n") :: [[String]]

	let lines = map (\str -> head str
		=~ "setTile\\(([0-9]+),([0-9]+),([a-z]+),([0-9]+),([0-9]+)\\)" :: [[String]]
		) test

	let t = map (newTileAnswer . map newTile) lines

	print t
