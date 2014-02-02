--{-# LANGUAGE NamedFieldPuns #-}
module Game.Map where

--	Cell(..), Map, MapConfig(..), CellType(..), CellTile,
--	mapNumCellsWidth, mapNumCellsHeight, mapNumCells,
--	mapNew, mapConfig, mapCell,

--	clipNeighborhood, cyclicNeighborhood
--	) where

--import qualified Data.Map as Map
--import Data.Maybe
--import Data.Int

--data Position = Position 
--	{ posX :: Int
--	, posY :: Int
--	} deriving (Eq, Ord, Show)

--type CellTile = Int32 -- from tiled

--data Cell = Cell 
--	{ cellPos :: Position
--	} deriving (Eq, Ord, Show)

--newCell :: CellTile -> Position -> Cell
--newCell tile pos = Cell
--	{ cellPos = pos
--	}

--data MapConfig = MapConfig
--	{ mapWidth :: Int
--	, mapHeight :: Int
--	, mapNeighborhoodFunc :: MapConfig -> NeighborhoodFunc
--	, mapCellTiles :: Map.Map (Int, Int) CellTile
--	}

--instance Show MapConfig where
--	show MapConfig { mapWidth, mapHeight } = show mapWidth ++ " / " ++ show mapHeight

--type LayerId = Int
--data MapLayer = MapLayer
--	{ layerId :: LayerId
--	, layerTiles :: Map.Map (Int, Int) CellTile
--	}

--data Map = Map
--	{ mapCells :: [[Cell]]
--	, mapConfig :: MapConfig

--	, mapLayers :: [MapLayer]

--	-- accessed using wrapper function mapNeighbors
--	, _mapNeighbors :: Map.Map Cell [Cell]
--	} deriving (Show)

--mapNumCells :: Map -> Int
--mapNumCells m = mapNumCellsWidth m * mapNumCellsHeight m

--mapNumCellsWidth :: Map -> Int
--mapNumCellsWidth map@Map { mapCells } = case mapNumCellsHeight map of
--	0 -> 0
--	_ -> length . head $ mapCells

--mapNumCellsHeight :: Map -> Int
--mapNumCellsHeight Map { mapCells } = length mapCells

--mapCell :: (Int, Int) -> Map -> Cell
--mapCell (x, y) m = (mapCells m)!!y!!x

--cyclicNeighborhood :: MapConfig -> NeighborhoodFunc
--cyclicNeighborhood MapConfig { mapWidth, mapHeight } = (\(Cell { cellPos, cellTile }) -> map (\f -> newCell cellTile . f $ cellPos) funcs)
--	where
--		funcs = [topPos, topRightPos, rightPos, bottomRightPos, bottomPos, bottomLeftPos, leftPos, topLeftPos]

--		leftPos (Position x y) = Position (left x) y
--		rightPos (Position x y) = Position (right x) y
--		topPos (Position x y) = Position x (top y)
--		bottomPos (Position x y) = Position x (bottom y)
--		topLeftPos = topPos . leftPos 
--		topRightPos = topPos . rightPos
--		bottomRightPos = bottomPos . rightPos
--		bottomLeftPos = bottomPos . leftPos

--		left x | x == 0 = mapWidth-1
--					| otherwise = x - 1
--		right x | x == mapWidth-1 = 0
--					 | otherwise = x + 1
--		top y | y == 0 = mapHeight-1
--				   | otherwise = y - 1
--		bottom y | y == mapHeight-1 = 0
--				   | otherwise = y + 1


--clipNeighborhood :: MapConfig -> NeighborhoodFunc
--clipNeighborhood MapConfig { mapWidth, mapHeight } = (\(Cell { cellPos, cellTile }) -> map (newCell cellTile) $ catMaybes (map ($cellPos) funcs))
--	where
--		funcs = [topPos, topRightPos, rightPos, bottomRightPos, bottomPos, bottomLeftPos, leftPos, topLeftPos]

--		leftPos (Position x y) | x == 0 = Nothing
--							   | otherwise = Just $ Position (x-1) y
--		rightPos (Position x y) | x == mapWidth-1 = Nothing
--							   | otherwise = Just $ Position (x+1) y	
--		topPos (Position x y) | y == 0 = Nothing
--							  | otherwise = Just $ Position x (y-1)							   
--		bottomPos (Position x y) | y == mapHeight-1 = Nothing
--								 | otherwise = Just $ Position x (y+1)

--		topLeftPos pos = topPos pos >>= leftPos 
--		topRightPos pos = topPos pos >>= rightPos
--		bottomRightPos pos = bottomPos pos >>= rightPos
--		bottomLeftPos pos = bottomPos pos >>= leftPos

--type NeighborhoodFunc = Cell -> [Cell]

--mapNew :: MapConfig -> Map
--mapNew config@MapConfig { mapWidth, mapHeight, mapNeighborhoodFunc, mapCellTiles } = Map
--	{ mapCells = cells
--	, mapConfig = config
--	, _mapNeighbors = neighbors
--	}
--	where
--		cells = [[newCell (mapCellTiles Map.! (x, y)) (Position x y) | x <- [0..mapWidth-1]] | y <- [0..mapHeight-1]]
--		neighbors = Map.fromList neighborFuncs

--		neighborFuncs = [(cell, mapNeighborhoodFunc config cell) | cell <- concat cells]

--mapNeighbors :: Map -> Cell -> [Cell]
--mapNeighbors Map { _mapNeighbors } cell = _mapNeighbors Map.! cell

