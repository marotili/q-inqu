{-# LANGUAGE TemplateHaskell #-}


module Game.World.Gen.Terrain where

import Debug.Trace
import Control.Lens
import Data.List
import qualified Data.Map as Map

data BaseTileType =
	  Wall
	| Floor
	| Door
	| NoMatch
	| Outside
	deriving (Show, Eq)

data StepTileType = 
	  WallS
	| WallSW
	| WallW
	| WallNW
	| WallN
	| WallNE
	| WallE
	| WallSE
	| WallInnerNW
	| WallInnerNE
	| WallInnerSE
	| WallInnerSW 
	| WallCenter
	deriving (Show, Eq)

data TileType = 
	  WallSW1
	| WallS1
	| WallSE1

	| WallS2
	| WallW2
	| WallE2
	| WallSW2
	| WallSE2

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

	| FinalFloor
	| FinalNoMatch
	 deriving (Show, Eq)

data GenMap = GenMap
	{ _mapBaseCells :: Map.Map (Int, Int) BaseTileType
	, _mapLevel1Cells :: Map.Map (Int, Int) StepTileType
	, _mapLevel2Cells :: Map.Map (Int, Int) StepTileType
	, _mapLevel3Cells :: Map.Map (Int, Int) StepTileType
	, _mapCompiledCells :: Map.Map (Int, Int) TileType
	, _mapWidth :: Int
	, _mapHeight :: Int
	} deriving (Show, Eq)

makeLenses ''GenMap

neighbors (x, y) = 
	[ (x - 1, y - 1), (x, y - 1), (x+1, y -1)
	, (x -1 , y), (x + 1, y)
	, (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)
	]

cellNeighborTypes :: GenMap -> (Int, Int) -> [BaseTileType]
cellNeighborTypes gm (x, y) = map (\(nx, ny) -> if 
		Map.member (nx, ny) (gm^.mapBaseCells) 
			then (gm^.mapBaseCells) Map.! (nx, ny)
			else Outside -- if ny > y then Floor else Wall -- up = wall / down =
	) neighborPos
	where
		neighborPos = neighbors (x, y)

newGenMap :: GenMap 
newGenMap = GenMap
	{ _mapBaseCells = Map.empty
	, _mapLevel1Cells = Map.empty
	, _mapLevel2Cells = Map.empty
	, _mapLevel3Cells = Map.empty
	, _mapCompiledCells = Map.empty
	, _mapWidth = 0
	, _mapHeight = 0
	}

tileLayer gm (x, y) = if
		((topTileType `elem` [WallN3, WallNW3, WallNE3] && not (tileType `elem` [WallS3, WallSE3, WallSW3]))
		|| tileType `elem` [WallN3, WallNW3, WallNE3]) || tileType == WallCenter3
	then "TopLayer"
	else "BottomLayer"
	where
		Just tileType = gm^.mapCompiledCells . at (x, y)
		Just topTileType = if Map.member (x, y-1) (gm^.mapCompiledCells)
			then gm^.mapCompiledCells . at (x, y-1)
			else Just FinalNoMatch



bottomTiles :: GenMap -> Map.Map (Int, Int) BaseTileType
--bottomTiles gm = Map.filterWithKey (\(x, y) tile -> tileLayer gm (x, y) == "BottomLayer" && tile /= FinalFloor) (gm^.mapCompiledCells)
bottomTiles gm = (Map.filter (\tile -> tile == Wall) $ gm^.mapBaseCells)

-- TODO: refactor
data Collect = Collect 
	{ _collectLastPos :: (Int, Int)
	, _collectStartPos :: (Int, Int)
	, _collectMaxX :: Int
	, _collectMaxY :: Int
	, _collectBoundary :: ((Float, Float), (Float, Float))
	, _collectFinished :: Bool
	} deriving (Show)

makeLenses ''Collect

tileBoundaries gm = map (^.collectBoundary) $ loop (bottomTiles gm) [] 
	where
		loop tileMap collection = let (collected, newTileMap) = matchBox tileMap in traceShow (newTileMap) $
			if newTileMap == Map.empty 
				then collection ++ collected
				else loop newTileMap (collected ++ collection)

		sortOrder (x1, y1) (x2, y2)
			| y1 < y2 = LT
			| y1 == y2 && x1 < x2 = LT
			| otherwise = GT

		-- collect box then remove the box and continue to collect
		matchBox tileMap = (collected, newMap)
			where
				-- reverse because foldr starts on the right side and we ordered descending
				collected = foldr collect [] $ reverse $ sortBy sortOrder $ Map.keys $ tileMap
				newMap = case collected of
					[] -> Map.empty
					_ -> removeMatched (head collected) tileMap

		-- remove the collected
		removeMatched col tileMap = foldr (Map.delete) tileMap between
			where
				between = [(x, y) | 
					  x <- [col^.collectStartPos._1 .. (col^.collectLastPos._1)]
					, y <- [col^.collectStartPos._2 .. (col^.collectLastPos._2)]
					]

		collect (x, y) [] = [Collect 
			{ _collectStartPos=(x,y)
			, _collectLastPos=(x,y)
			, _collectMaxX = x
			, _collectMaxY = y
			, _collectBoundary=((xf*24, yf*24), ((xf+1)*24, (yf+1)*24))
			, _collectFinished = False
			}]
			where
				xf = fromIntegral x
				yf = fromIntegral y
		collect (x, y) (col:cs)
			-- same line, advance one
			| x == lx + 1 && y == ly && y == oy = (col 
					& collectLastPos .~ (x, y)
					& collectBoundary._2._1 %~ (+24) --(\((x1, y1), (x2, y2)) -> ((x1, y1), ())
					& collectMaxX .~ x
				):cs

			| x > (col^.collectMaxX) = col : cs

			-- next line, block continues
			| lx == (col^.collectMaxX) && x == ox && y == ly + 1 = (col
					& collectLastPos .~ (x, y)
					& collectBoundary._2._2 %~ (+24)
					& collectMaxY .~ y
				):cs

			-- advance new line
			| y == ly && x == lx + 1 = (col
					& collectLastPos .~ (x, y)
				):cs

			| y > (col^.collectMaxY) = col : cs

			| (col^.collectFinished) = col : cs

			-- line could not be finished
			| otherwise = (col
					& collectBoundary._2._2 %~ (\y -> y - 24)
					& collectLastPos._1 .~ (col^.collectMaxX)
					& collectLastPos._2 -~ 1
					& collectFinished .~ True
				): cs
			--Collect 
			--	{ _collectStartPos=(x,y)
			--	, _collectLastPos=(x,y)
			--	, _collectBoundary=((xf*24, yf*24), ((xf+1*24), (yf+1)*24))
			--	} : col : cs
			where
				(ox, oy) = col^.collectStartPos
				(lx, ly) = col^.collectLastPos
				w = gm^.mapWidth
				h = gm^.mapHeight
				xf = fromIntegral x
				yf = fromIntegral y
			 
testBound = do
	let ds = tileBoundaries mkGenWorld
	mapM_ print ds
	--print $ sortBy sortOrder $ Map.keys $ bottomTiles mkGenWorld
	where
		sortOrder (x1, y1) (x2, y2)
			| y1 < y2 = LT
			| y1 == y2 && x1 < x2 = LT
			| otherwise = GT
world = 
	[ [Wall, Wall, 	Wall,  Wall, Wall,  Wall,  Wall,  Wall,  Wall,  Wall, Wall,  Wall,   Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Wall, Wall, Wall, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Wall,  Wall,  Wall,  Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Wall,  Wall,  Wall,  Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Floor, Floor, Floor,Floor, Floor, Floor, Floor, Floor, Floor,Floor, Floor,  Wall]
	, [Wall, Wall, 	Wall,  Wall, Wall,  Wall,  Wall,  Wall,  Wall,  Wall, Wall,  Wall,   Wall]
	]

newMapFromList lists = newGenMap 
	& mapBaseCells .~ foldr (uncurry Map.insert) Map.empty
		[((x, y), tileType) | (y, line) <- zip [0..] lists, (x, tileType) <- zip [0..] line]
	& mapWidth .~ (length (lists!!0))
	& mapHeight .~ (length lists)

uncurry8 func [a, b, c, d, e, f, g, h] = func a b c d e f g h


matchStart Floor Floor Floor
			Floor Floor
			Floor Floor Floor = error "Single walls are not supported"
matchStart Wall Floor Floor
			Floor Floor
			Floor Floor Floor = error "Diagonal walls are not supported"
matchStart Wall Wall Floor
			Floor Floor
			Floor Floor Floor = error "Single walls are not supported"
matchStart Wall Wall Wall
			Floor Floor
			Floor Floor Floor = error "Single walls are not supported"

matchStart Wall Wall Wall
			Wall 	Wall
			_ Floor _ = WallS

matchStart _ Floor _
			Wall 	Wall
			Wall Wall Wall = WallN

matchStart _ Wall Wall
			Floor Wall
			_ Wall Wall = WallW

matchStart Wall Wall _
			Wall Floor
			Wall Wall _ = WallE

matchStart _ Wall Wall
			Floor Wall
			Floor Floor Floor = WallSW

matchStart Wall Wall _
		 	Wall Floor
		 	Floor Floor Floor = WallSE

matchStart Floor Floor Floor
			Floor Wall
			_ Wall Wall = WallNW

matchStart Floor Floor Floor
			Wall Floor
			Wall Wall _ = WallNE

-- inner corners
matchStart Wall Wall Wall
			Wall Wall
			Wall Wall Floor = WallInnerNW

matchStart Wall Wall Wall
			Wall Wall
			Floor Wall Wall = WallInnerNE

matchStart Wall Wall Floor
			Wall Wall
			Wall Wall Wall = WallInnerSW

matchStart Floor Wall Wall
			Wall Wall
			Wall Wall Wall = WallInnerSE

matchStart Wall Wall Wall
			Wall Wall
			Wall Wall Wall = WallCenter


-- borders (otherwise not allowed)
matchStart Outside Outside Outside
			Outside Wall
			Outside Wall Floor = WallInnerNW
matchStart Outside Outside Outside
			Wall Outside
			Floor Wall Outside = WallInnerNE
matchStart Outside Wall Floor
			Outside Wall
			Outside Outside Outside = WallInnerSW
matchStart Floor Wall Outside
			Wall Outside
			Outside Outside Outside = WallInnerSE

matchStart Outside Wall _
			Outside _	
			Outside Wall _ = WallE

matchStart _ Wall Outside
			_ Outside	
			_ Wall Outside = WallW

matchStart _ _ _
			Wall Wall	
			Outside Outside Outside = WallN

matchStart Outside Outside Outside
			Wall Wall	
			_ _ _ = WallS

matchStart w1 w2 w3 w4 w5 w6 w7 w8 = error $ 
	show [w1, w2, w3, w4, w5, w6, w7, w8]

step :: GenMap -> GenMap
step gm = gm 
		& mapLevel1Cells .~ newCells
		& mapLevel2Cells .~ newCells2
		& mapLevel3Cells .~ newCells3
	where
		cellPos = map fst $ filter (\(_, tileType) -> tileType == Wall) $ 
			Map.toList (gm^.mapBaseCells)
		cellNeighbors = map neighbors cellPos

		newCells :: Map.Map (Int, Int) StepTileType
		newCells = foldr (uncurry Map.insert . applyRule 0) Map.empty cellPos

		newCells2 :: Map.Map (Int, Int) StepTileType
		newCells2 = foldr (uncurry Map.insert . applyRule 1) Map.empty cellPos

		newCells3 :: Map.Map (Int, Int) StepTileType
		newCells3 = foldr (uncurry Map.insert . applyRule 2) Map.empty cellPos

		applyRule :: Int -> (Int, Int) -> ((Int, Int), StepTileType)
		applyRule offset (cpx, cpy) = (
				(cpx, cpy-offset), uncurry8 matchStart cellNeighbors
			)
			where
				cellNeighbors :: [BaseTileType]
				cellNeighbors = cellNeighborTypes gm (cpx, cpy)

compile :: GenMap -> GenMap
compile gm = gm
	& mapCompiledCells .~ step4
	where
		step0 = foldr (\(k, a) -> Map.insert k (level0 a)) Map.empty $ Map.toList (gm^.mapBaseCells)

		step1 = foldr (\(k, a) m -> case level1 a of
				FinalNoMatch -> Map.insert k FinalNoMatch m
				a' -> Map.insert k a' m
			) step0 $ Map.toList (gm^.mapLevel1Cells)

		step2 = foldr (\(k, a) m -> case level2 a of
				FinalNoMatch -> Map.insert k FinalNoMatch m
				a' -> Map.insert k a' m
			) step1 $ Map.toList (gm^.mapLevel2Cells)
		step3 = foldr (\(k, a) -> Map.insert k (level3 a)) step2 $ Map.toList (gm^.mapLevel3Cells)
		step4 = Map.map (\a -> if a == FinalNoMatch then WallCenter3 else a) step3

		level0 Floor = FinalFloor
		level0 _ = FinalNoMatch

		level1 :: StepTileType -> TileType
		level1 WallS = WallS1
		level1 WallSW = WallSW1
		level1 WallSE = WallSE1
		level1 _ = FinalNoMatch

		level2 WallW = WallW2
		level2 WallE = WallE2
		level2 WallS = WallS2
		level2 WallSW = WallSW2
		level2 WallSE = WallSE2
		level2 _ = FinalNoMatch

		level3 WallN = WallN3
		level3 WallNE = WallNE3
		level3 WallE = WallE3
		level3 WallSE = WallSE3
		level3 WallS = WallS3
		level3 WallSW = WallSW3
		level3 WallW = WallW3
		level3 WallNW = WallNW3
		level3 WallCenter = WallCenter3

		level3 WallInnerNW = WallOuterNW3
		level3 WallInnerNE = WallOuterNE3
		level3 WallInnerSW = WallOuterSW3
		level3 WallInnerSE = WallOuterSE3
		--level3 _ = FinalNoMatch

mkGenWorld = m2
	where
		m = newMapFromList world
		m' = step m
		m2 = compile m'

test = do
	let cells =  (mkGenWorld^.mapCompiledCells)
	mapM_ (\(xs, y) -> print [cells Map.! (x, y) | x <- xs]) [([0..12], y) | y <- [-2..13]]