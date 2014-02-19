{-# LANGUAGE TemplateHaskell #-}


module Game.World.Gen.Terrain where

import Control.Lens
import qualified Data.Map as Map

data TerrainType = 
	  TerrainWall
	| TerrainFloor
 	deriving (Show)

data Tileset = Tileset
	{ tilesetName :: String
	} deriving (Show)

isWallOrBorder tileType = not (tileType == Floor || tileType == Door)
isWall tileType = not (tileType == Floor || tileType == Door || tileType == NoMatch)

data TileType = 
	  WallSW1
	| WallS1
	| WallSE1

	| WallS2
	| WallW2
	| WallE2

	| WallN3
	| WallNE3
	| WallE3
	| WallSE3
	| WallS3
	| WallSW3
	| WallW3
	| WallNW3
	| WallCenter3

	| WallOuterNW3
	| WallOuterNE3
	| WallOuterSW3
	| WallOuterSE3

	| Wall
	| Floor
	| Door
	| NoMatch
	 deriving (Show, Eq)

rule _ Wall Wall 
	 Floor Wall 
	 Floor Floor Floor = WallSW1 

rule Wall  Wall  _ 
	 Wall 		 Floor 
	 Floor Floor Floor = WallSE1

rule Wall  Wall  Wall
	 Wall  		 Wall
	 Floor Floor Floor = WallS1

rule Wall Wall Wall
	Wall Wall
	Floor Floor Wall = WallSE1

rule Wall Wall Wall
	Wall Wall
	Wall Floor Floor = WallSW1

rule Wall Wall Wall
	Wall Wall
	Floor Wall Wall = WallSE1

rule Wall Wall Wall
	Wall Wall
	Floor Wall Wall = WallSW1

rule _ _  _
	 _ 		 _
	 _ WallSW1 _ = WallW2

rule _ _  _
	 _ 		 _
	 _ WallSE1 _ = WallE2

rule _ _ _
	 _	 _
	 _ WallS1 _ = WallS2

rule Floor w2 w3
	 Floor w1
	 Floor WallW2 w4
	 	| isWall w1 && isWall w2 && isWall w3 && isWall w4 = WallW3

rule w5 w3 w4
	 Floor w2
	 Floor WallW2 w1
	 	| isWall w1 && isWall w2 && isWall w3 && isWall w4 && isWall w5 = WallW3

rule w3 w4 Floor
	 w2 Floor
	 w1 WallE2 Floor	 	
	 	| isWall w1 && isWall w2 && isWall w3 && isWall w4 = WallE3

rule w3 w4 w5
	 w2 Floor
	 w1 WallE2 Floor
	 	| isWall w1 && isWall w2 && isWall w3 && isWall w4 && isWall w5 = WallE3

-- borders
rule NoMatch NoMatch NoMatch
	 w1 w2
	 _ Floor _ | isWall w1 && isWall w2 = WallS3

rule _ Floor _
	 w1 w2
	 NoMatch NoMatch NoMatch | isWall w1 && isWall w2 = WallN3

rule NoMatch w1 _
	 NoMatch Floor
	 NoMatch w2 _ | isWall w1 && isWall w2 = WallE3

rule _ w1 NoMatch
	 Floor NoMatch
	 _ w2 NoMatch | isWall w1 && isWall w2 = WallW3

rule NoMatch NoMatch NoMatch
	 NoMatch w1
	 NoMatch w2 Floor | isWall w1 && isWall w2 = WallOuterNW3

rule NoMatch NoMatch NoMatch
	 NoMatch w1
	 NoMatch w2 w3 | isWall w1 && isWall w2 && isWall w3 = WallCenter3

rule NoMatch NoMatch NoMatch
	 w1 NoMatch
	 Floor w2 NoMatch | isWall w1 && isWall w2 = WallOuterNE3

rule NoMatch NoMatch NoMatch
	 w1 NoMatch
	 w3 w2 NoMatch | isWall w1 && isWall w2 && isWall w3 = WallCenter3

rule Floor w1 NoMatch
	 w2 NoMatch
	 NoMatch NoMatch NoMatch | isWall w1 && isWall w2 = WallOuterSE3

rule w3 w1 NoMatch
	 w2 NoMatch
	 NoMatch NoMatch NoMatch | isWall w1 && isWall w2 && isWall w3 = WallCenter3

rule NoMatch w2 Floor
	 NoMatch w1
	 NoMatch NoMatch NoMatch | isWall w1 && isWall w2 = WallOuterSW3

rule NoMatch w2 w3
	 NoMatch w1
	 NoMatch NoMatch NoMatch | isWall w1 && isWall w2 && isWall w3 = WallCenter3

rule w1 w2 w3
	 w4 w5
	 Floor w6 w7 | all isWallOrBorder [w1, w2, w3, w4, w5, w6, w7] = WallSE1

rule w1 w2 w3
	 w4 w5
	 w7 w6 Floor | all isWallOrBorder [w1, w2, w3, w4, w5, w6, w7] = WallSW1

rule w1 w2 w3
	 w4 w5
	 w7 Floor w6 | all isWallOrBorder [w1, w2, w3, w4, w5, w6, w7] = WallS1

rule w1 w2 w3
	 w4 	w5
	 w6 w7 w8
	 	| all isWallOrBorder [w1, w2, w3, w4, w5, w6, w7, w8] = WallCenter3
	 	| otherwise = NoMatch -- error "unmatched center"

rule _ _ _ _ _ _ _ _ = NoMatch

data GenMap = GenMap
	{ _mapCells :: Map.Map (Int, Int) TileType
	} deriving (Show, Eq)
makeLenses ''GenMap

neighbors (x, y) = 
	[ (x - 1, y - 1), (x, y - 1), (x+1, y -1)
	, (x -1 , y), (x + 1, y)
	, (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
	]

cellNeighborTypes :: GenMap -> (Int, Int) -> [TileType]
cellNeighborTypes gm (x, y) = map (\(x, y) -> if 
		Map.member (x, y) (gm^.mapCells) 
			then (gm^.mapCells) Map.! (x, y)
			else NoMatch -- indicates border
	) neighborPos
	where
		neighborPos = neighbors (x, y)

newGenMap :: GenMap 
newGenMap = GenMap
	{ _mapCells = Map.empty
	}

world = 
	[ [Wall, Wall, Wall, Wall, Wall]
	, [Wall, Wall, Wall, Wall, Wall]
	, [Wall, Wall, Wall, Wall, Wall]
	, [Wall, Floor, Wall, Wall, Wall]
	, [Wall, Floor, Wall, Wall, Wall]
	, [Wall, Floor, Wall, Wall, Wall]
	, [Wall, Floor, Floor, Floor, Wall]
	, [Wall, Wall, Wall, Wall, Wall]
	]

newMapFromList lists = newGenMap 
	& mapCells .~ foldr (uncurry Map.insert) Map.empty
		[((x, y), tileType) | (y, line) <- zip [0..] lists, (x, tileType) <- zip [0..] line]

uncurry8 func [a, b, c, d, e, f, g, h] = func a b c d e f g h

step :: GenMap -> GenMap
step gm = gm & mapCells .~ newCells
	where
		cellPos = map fst $ Map.toList (gm^.mapCells)
		cellNeighbors = map neighbors cellPos

		newCells :: Map.Map (Int, Int) TileType
		newCells = foldr (uncurry Map.insert . applyRule) Map.empty cellPos

		applyRule :: (Int, Int) -> ((Int, Int), TileType)
		applyRule cellPos = (cellPos
			, case uncurry8 rule cellNeighbors of
					NoMatch -> (gm^.mapCells) Map.! cellPos
					otherwise -> uncurry8 rule cellNeighbors
			)
			where
				cellNeighbors :: [TileType]
				cellNeighbors = cellNeighborTypes gm cellPos

test = do
	let m = newMapFromList world
	let m' = step m
	let m2 = step m'
	let m3 = step m2
	let m4 = step m3

	print $ m2 == m'
	print $ m3 == m2
	print $ m4 == m3
	print m3
	return ()