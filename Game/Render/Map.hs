{-# LANGUAGE NamedFieldPuns #-}
module Game.Render.Map where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Ptr

import Debug.Trace

import Linear

import Data.Int
--import Linear.Matrix

import qualified Data.Vector.Storable as V
import qualified Game.Map as Game

import Game.Render.Render


data Map = Map
	{ mapTopLeft :: V2 Float
	, tileSize :: Float
	, gameMap :: Game.Map
	}

newMap :: Game.Map -> Map
newMap gameMap = Map
	{ mapTopLeft = V2 0 0
	, tileSize = 50
	, gameMap = gameMap
	}

data Tile = Tile (V.Vector Float)

newTile :: Map -> Tile
newTile m = Tile $ V.fromList
	[ 0, 0
	, dx, 0
	, dx, dy
	, dx, dy
	, 0, dy
	, 0, 0
	]
	where
		(dx, dy) = (tileSize m, tileSize m)

indexToCoords :: Game.MapConfig -> Int -> (Int, Int)
indexToCoords gameMap idx = (idx `mod` Game.mapWidth gameMap, idx `div` Game.mapHeight gameMap)

tileColors :: Map -> V.Vector Int32
tileColors m = V.fromList $ colors
	where
		gm = (gameMap m)
		colors = map (cellTypeToInt32 . Game.cellType) cells
		cells = map (\coord -> Game.mapCell coord gm) coords
		coords = [indexToCoords (Game.mapConfig gm) idx | idx <- [0..Game.mapNumCells gm-1]]

		cellTypeToInt32 :: Game.CellType -> Int32
		cellTypeToInt32 Game.CellTypeGrass = 0
		cellTypeToInt32 Game.CellTypeForest = 1
		cellTypeToInt32 _ = 9

tileCoords :: Map -> V.Vector Float
tileCoords m = V.fromList $ foldr (\(a, b) l -> a : b : l) [] (tileCoords' m)

tileCoords' m = zipWith applyOffsets coords offsets
	where
		w = Game.mapNumCellsWidth (gameMap m)
		h = Game.mapNumCellsHeight (gameMap m)

		o = (tileSize m)*0.1
		offsets :: [(Float, Float)]
		offsets = [(o*(fromIntegral x), o*(fromIntegral y)) | y <- [0..h-1], x <- [0..w-1]]
		coords = [(x, y) | 
				y <- [y0, (y0+tileSize m) .. y1-(tileSize m)],
				x <- [x0, x0 + (tileSize m) .. x1-(tileSize m)]
			]

		applyOffsets :: (Float, Float) -> (Float, Float) -> (Float, Float)
		applyOffsets (x, y) (ox, oy) = (x+ox, y+oy)

		V2 x0 y0 = mapTopLeft m
		V2 x1 y1 = mapBottomRight m

mapBottomRight :: Map -> V2 Float 
mapBottomRight Map { mapTopLeft, tileSize, gameMap } =
	mapTopLeft + numTiles * V2 tileSize tileSize
	where numTiles = V2 (fromIntegral $ Game.mapNumCellsWidth gameMap) (fromIntegral $ Game.mapNumCellsHeight gameMap)

mapSize :: Map -> V2 Float
mapSize m = (mapBottomRight m - mapTopLeft m)

-- input in render world coordinates
mapSelectTile :: V2 Float -> Map -> Maybe (Int, Int)
mapSelectTile (V2 x y) m = case length topLeft of
	0 -> Nothing
	_ -> Just $ fst . head $ topLeft
	where
		coords = [(x, y) | 
			y <- [0, 1.. (Game.mapNumCellsHeight . gameMap $ m)-1],
			x <- [0, 1.. (Game.mapNumCellsWidth . gameMap $ m)-1]
			]
		bottomRight = filter ((\(x', y') -> (x' + ts > x) && (y' + ts > y)) . snd) $ zip coords (tileCoords' m)
		topLeft = filter ((\(x', y') -> (x' < x) && (y' < y)) . snd) bottomRight
		ts = tileSize m

test = do
	let m = Game.mapNew Game.MapConfig { 
		Game.mapWidth = 9, Game.mapHeight = 9, 
		Game.mapNeighborhoodFunc = Game.clipNeighborhood
	}
	let rm = newMap m
	print (tileCoords rm)
	print $ indexToCoords (Game.mapConfig m) 80

data WorldRenderContext = WorldRenderContext
	{ wrcVao :: GL.VertexArrayObject
	--, wrcMeshBuffer :: V.Vector Float
	, wrcMeshBuffer :: GL.BufferObject
	--, wrcCellTypes :: V.Vector Int32
	--, wrcCellCoords :: V.Vector Float
	, wrcCoordSSB :: GL.BufferObject
	, wrcTypeSSB :: GL.BufferObject
	, wrcNumTiles :: Int
	}

newWorldRenderContext gameMap = do
	let m = newMap gameMap
	buffers <- GL.genObjectNames 3 :: IO [GL.BufferObject]
	[vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]

	print $ tileCoords m
	print $ tileColors m

	let (Tile tileData) = newTile m
	uploadFromVec GL.ArrayBuffer (buffers!!0) tileData
	print $ tileData

	uploadFromVec GL.ShaderStorageBuffer (buffers!!1) (tileCoords m)
	uploadFromVec GL.ShaderStorageBuffer (buffers!!2) (tileColors m)

	return WorldRenderContext
		{ wrcVao = vao
		, wrcMeshBuffer = (buffers!!0)
		, wrcCoordSSB = (buffers!!1)
		, wrcTypeSSB = (buffers!!2)
		, wrcNumTiles = Game.mapNumCells gameMap
		}

bindWorldRenderContext wrc program = do
	GL.bindVertexArrayObject $= (Just $ wrcVao wrc)	
	mesh <- GL.get (GL.attribLocation program "mesh")

 	GL.vertexAttribArray mesh $= GL.Enabled
 	GL.bindBuffer GL.ArrayBuffer $= Just (wrcMeshBuffer wrc)
 	GL.vertexAttribPointer mesh $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)

-- all static for now
--updateRenderContext wrc = do
	--updateFromVec GL.ShaderStorageBuffer (wrcTypeSSB) (V.fromList $ cellTypes world)

renderWorldRenderContext program wrc = do
	GL.bindBufferBase' GL.ShaderStorageBuffer 2 (wrcCoordSSB wrc)
	GL.shaderStorageBlockBinding program 2 2

	GL.bindBufferBase' GL.ShaderStorageBuffer 0 (wrcTypeSSB wrc)
	GL.shaderStorageBlockBinding program 0 0

	GL.drawArraysInstanced GL.Triangles 0 6 (fromIntegral (wrcNumTiles wrc))

