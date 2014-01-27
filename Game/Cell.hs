{-# LANGUAGE TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module Game.Cell (Cell, World, newPos, newWorld, getTypes, forestSystem, worldApplyUpdate, worldSetForest, execState, toState) where

import GHC.Conc (numCapabilities)
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import Data.List
import Control.Monad.State
import Control.Lens
import qualified Control.Arrow as A
import Data.Maybe
import Data.Int
import Data.Array
import qualified Data.Array as Array (indices, index)

import Control.Parallel.Strategies
import Control.Parallel

import System.Random

import Numeric.Noise.Perlin

data Cell = Cell { cellId :: Integer
				 } deriving (Eq, Show, Ord)

--data CellType = TypeGrass | TypeForest | TypeWater deriving (Eq, Show)
type CellType = Int32
typeGrass = 0
typeForest = 1
typeWater = 2
	
data Position = Position Integer Integer deriving (Eq, Show, Ord)

newPos x y = Position x y

worldBound = worldSize-1
worldSize = 64

left x | x == 0 = worldBound
	   | otherwise = x - 1
right x | x == worldBound = 0
		| otherwise = x + 1
top y | y == 0 = worldBound
	  | otherwise = y - 1
bottom y | y == worldBound = 0
		 | otherwise = y + 1

topLeftPos = topPos . leftPos
topPos (Position x y) = Position x (top y)
topRightPos = topPos . rightPos
rightPos (Position x y) = Position (right x) y
bottomRightPos = bottomPos . rightPos
bottomPos (Position x y) = Position x (bottom y)
bottomLeftPos = bottomPos . leftPos
leftPos (Position x y) = Position (left x) y

-- apply argument pos to list of functions
neighborPositions :: Position -> [Position]
neighborPositions pos = map (\x -> x pos) [topLeftPos, topPos, topRightPos, rightPos, bottomRightPos, bottomPos, bottomLeftPos, leftPos]

data Forest = Forest 
	{ forestSize :: Integer
	} deriving (Eq, Show)

data Water = Water
	{ waterAmount :: Integer
	} deriving (Eq, Show)

data Height = Height {
	height :: Integer
	} deriving (Eq, Show)

instance Ix Cell where
	range (Cell id0, Cell id1) = map Cell $ range (id0, id1)
	index (Cell id0, Cell id1) (Cell id2) = Array.index (id0, id1) id2
	inRange (Cell id0, Cell id1) (Cell id2) = inRange (id0, id1) id2
	rangeSize (Cell id0, Cell id1) = rangeSize (id0, id1)

data World = World
	{ worldCells :: Array Cell Position
	, _worldNeighbors :: Array Cell [Cell]
	, _worldCellTypes :: Array Integer CellType
	, _worldForestNeighborsNotForests :: Set.Set Cell
	, _worldForests :: Map.Map Cell Forest
	, _worldHeight :: Map.Map Cell Height
	, _worldWaters :: Map.Map Cell Water
	} deriving (Show)

makeLenses ''World

class Key a where
	cellKey :: a -> World -> Cell

instance Key Cell where
	cellKey cell _ = cell

instance Key Position where
	cellKey pos world = posToCell pos

{-# INLINE cellToPos #-}
cellToPos :: World -> Cell -> Position
cellToPos world cell = (worldCells world) ! cell

{-# INLINE posToCell #-}
posToCell :: Position -> Cell
posToCell (Position x y) = Cell { cellId = y * worldSize + x + 1 }

cellNeighbors :: World -> Cell -> [Cell]
cellNeighbors world cell = (world^.worldNeighbors) ! cell
--cellNeighbors world cell = map (posToCell) (neighborPositions cellPosition)
	--where 
		--cellPosition = cellToPos world cell

cellListNeighborsSet world cells = Set.fromList $ concatMap (cellNeighbors world) cells

cellListNeighbors :: World -> [Cell] -> [Cell]
cellListNeighbors world cells = Set.toList $ cellListNeighborsSet world cells

-- return keys as list of a map
mapKeys :: Map.Map k a -> [k]
mapKeys m = map fst (Map.toList m)

cellIn :: (World -> Map.Map Cell a) -> World -> Cell -> Bool
cellIn getter world cell = Map.member cell (getter world)

-- returns a list with neighborcells that are forests
neighborsThatAreForests :: World -> Cell -> [Cell]
neighborsThatAreForests world cell = filter (cellIsForest world) (cellNeighbors world cell)

-- given a list of keys initialize map with key : defvalue
initializeMapFromList :: Ord k => a -> [k] -> Map.Map k a
initializeMapFromList defValue = foldl' (\world k -> Map.insert k defValue world) Map.empty

numForestsExpansion = 3
numForestDie = 1

forestCanGrow :: World -> Cell -> Bool
forestCanGrow world cell = numForestsExpansion <= length (neighborsThatAreForests world cell)
forestWillDie :: World -> Cell -> Bool
forestWillDie world cell = numForestDie >= length (neighborsThatAreForests world cell)

newForestFromList :: [Cell] -> Map.Map Cell Forest
newForestFromList cells = initializeMapFromList Forest { forestSize = 1 } cells

selectCells :: World -> (World -> Map.Map Cell a) -> [Cell]
selectCells world getter = mapKeys $ getter world

selectForests :: World -> [Cell]
selectForests world = selectCells world (^.worldForests)

--cellListNeighborsNotForest world cell = filter (not . cellIsForest world) (cellListNeighbors world cell)


cellIsForest world (Cell cellID) = typeForest == ((world^.worldCellTypes) ! cellID)
cellIsNotForest world (Cell cellID) = not (typeForest == ((world^.worldCellTypes) ! cellID))

neighborsNotForests :: World -> Cell -> [Cell]
neighborsNotForests world cell = neighborsOfCellWithCondition world cell (cellIsNotForest world)

optimizedCellListNeighborsNotForest :: World -> [Cell] -> [Cell]
optimizedCellListNeighborsNotForest world cells = neighborsOfCellsWithCondition world cells cond
	where cond = cellIsNotForest world

neighborsOfCellWithCondition world cell cond = filter cond (cellNeighbors world cell)
neighborsOfCellsWithCondition world cells cond = Set.toList $ Set.fromList neighborCells
	where neighborCells = concatMap (\cell -> neighborsOfCellWithCondition world cell cond) cells


data ForestUpdate = ForestUpdate
	{ updateNewForest :: Map.Map Cell Forest
	, updateDeleteForest :: [Cell]
	} deriving (Show)
forestSystem world = ForestUpdate { updateNewForest = newForestFromList growingForest, updateDeleteForest = [] }
	where
		growingForest = filter (forestCanGrow world) eligibleCells
		dyingForest = filter (forestWillDie world) eligibleDieCells

		-- only neighbors to forests can grow
		--forests = selectForests world
		--eligibleCells = optimizedCellListNeighborsNotForest world forests
		eligibleCells = Set.toList (world^.worldForestNeighborsNotForests)
		-- only forests can die
		eligibleDieCells = selectForests world

worldSetForest :: Key k => k -> State World ()
worldSetForest key = do
	world <- get
	let cell = cellKey key world
	worldForests .= Map.insert cell Forest { forestSize = 1 } (world^.worldForests)

	-- update cell types
	let updatedCellTypes = (world^.worldCellTypes) // [(cellId cell, typeForest)]
	worldCellTypes .= updatedCellTypes

	-- update container
	let neighbors = neighborsNotForests world cell
	let updatedNeihborsNotForests = Set.union (world^.worldForestNeighborsNotForests) $ Set.fromList neighbors
	worldForestNeighborsNotForests .= updatedNeihborsNotForests

hasForestNeighbor world cell = or $ map (cellIsForest world) (cellNeighbors world cell)

worldApplyUpdate :: ForestUpdate -> State World ()
worldApplyUpdate ForestUpdate { updateNewForest=newForest,
								updateDeleteForest=deleteForest} = do
	world <- get

	-- update forests
	let forestRemoved = Map.difference (world^.worldForests) (Map.fromList [(cell, Forest { forestSize = 1}) | cell <- deleteForest])
	let addedForests = Map.union newForest forestRemoved
	worldForests .= addedForests

	-- update types
	let newGrassTypes = [(cellId cell, typeGrass) | cell <- deleteForest]
	let newForestTypes = [(cellId cell, typeForest) | cell <- mapKeys newForest]
	worldCellTypes .= (world^.worldCellTypes) // (newGrassTypes ++ newForestTypes)

	-- update neighbors
	let notAnymoreNeighbors = filter (not . hasForestNeighbor world) (optimizedCellListNeighborsNotForest world deleteForest)
	let newNeighbors = optimizedCellListNeighborsNotForest world (mapKeys newForest)
	worldForestNeighborsNotForests .= foldr Set.delete (world^.worldForestNeighborsNotForests) notAnymoreNeighbors
	worldForestNeighborsNotForests .= foldr Set.insert (world^.worldForestNeighborsNotForests) newNeighbors

toState f = get >>= (return . f)

newWorld = World 
		{ worldCells = cells 
		, _worldCellTypes = grasses
		, _worldForests = Map.empty
		, _worldHeight = heights
		, _worldWaters = waters
		, _worldForestNeighborsNotForests = Set.empty
		, _worldNeighbors = neighbors
		}
	where
		--grasses = foldl' (\world cell -> Map.insert cell TypeGrass world) Map.empty sourceCells
		grasses = array (1, worldSize^2) [(cellId cell, typeGrass) | cell <- sourceCells]
		cells = array (Cell 1, Cell (worldSize^2)) [(cell, pos) | (cell, pos) <- cellList]
		--cells = foldl' (\world (cell, pos) -> Bimap.insert cell pos world) Bimap.empty cellList
		positions = [Position x y | x <- [0..worldSize-1], y <- [0..worldSize-1]]
		sourceCells = [Cell {cellId=cellId} | cellId <- [1..worldSize^2]]
		cellList = zip sourceCells positions

		heights = foldl' (\heightMap (cell, height) -> Map.insert cell height heightMap) Map.empty (zip sourceCells genHeight)
		waters = foldl' (\waterMap cell -> Map.insert cell Water { waterAmount = 1 } waterMap) Map.empty waterCells

		totalNumCells = worldSize^2
		percentWaterCells = 0.05
		numWaterCells = round $ (fromIntegral totalNumCells)*percentWaterCells
		waterCellIds = take numWaterCells $ (randomRs (0 :: Integer, totalNumCells) (mkStdGen 0))
		waterCells = map (\cellId -> Cell { cellId=cellId }) waterCellIds

		neighbors = array (Cell 1, Cell (worldSize^2)) [(cell, map posToCell (neighborPositions (cells ! cell))) | cell <- sourceCells]

genHeight = map makePositive [Height (round ((noise+1)*64)) | noise <- map (noiseValue noiseSource) coordinates] 
	where
		makePositive x | positive x = x
					   | otherwise = Height 0
		positive (Height x) = x >= 0
		noiseSource = perlin 1 5 0.05 0.5
		coordinates = [(x, y, 0) | x <- [0..worldBound], y <- [0..worldBound]]


cellType :: World -> Cell -> CellType
cellType world cell | Map.member cell (world^.worldForests) = typeForest
					| Map.member cell (world^.worldWaters) = typeWater
					| otherwise = typeGrass

getTypes :: World -> [Int32]
getTypes world = elems (world^.worldCellTypes)
