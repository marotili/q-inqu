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
	, tileSize :: V2 Float
	, gameMap :: Game.Map
	} deriving (Show)

newRenderMap :: Game.Map -> (Float, Float) -> Map
newRenderMap gameMap (tileSizeX, tileSizeY) = Map
	{ mapTopLeft = V2 0 0
	, tileSize = V2 tileSizeX tileSizeY
	, gameMap = gameMap
	}

data Tile = Tile (V.Vector Float)

data TileSet = TileSet
	{ tileSetImage :: FilePath
	, tileSetData :: TileSetData
	}

data TileSetData = TileSetData
	{ firstGid :: Int32
	, imageSize :: Int32
	, spacing :: Int32
	, margin :: Int32
	, tileWidth :: Int32
	, tileHeight :: Int32
	, padding0 :: Int32
	, padding1 :: Int32
	}

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
		V2 dx dy = tileSize m

indexToCoords :: Game.MapConfig -> Int -> (Int, Int)
indexToCoords gameMap idx = (idx `mod` Game.mapWidth gameMap, idx `div` Game.mapHeight gameMap)

tileColors :: Map -> V.Vector Int32
tileColors m = V.fromList $ colors
	where
		gm = (gameMap m)
		colors = map (Game.cellTile) cells
		cells = map (\coord -> Game.mapCell coord gm) coords
		coords = [indexToCoords (Game.mapConfig gm) idx | idx <- [0..Game.mapNumCells gm-1]]

		--cellTypeToInt32 :: Game.CellType -> Int32
		--cellTile x = x
		--cellTypeToInt32 Game.CellTypeForest = 1
		--cellTypeToInt32 _ = 9

tileCoords :: Map -> V.Vector Float
tileCoords m = V.fromList $ foldr (\(a, b) l -> a : b : l) [] (tileCoords' m)

tileCoords' m = zipWith applyOffsets coords offsets
	where
		w = Game.mapNumCellsWidth (gameMap m)
		h = Game.mapNumCellsHeight (gameMap m)

		(V2 tsx tsy) = tileSize m
		ox = tsx * 0.0
		oy = tsy * 0.0
		offsets :: [(Float, Float)]
		offsets = [(ox*(fromIntegral x), oy*(fromIntegral y)) | y <- [0..h-1], x <- [0..w-1]]
		coords = [(x, y) | 
				y <- [y0, y0+tsy .. y1-tsy],
				x <- [x0, x0 + tsx .. x1-tsx]
			]

		applyOffsets :: (Float, Float) -> (Float, Float) -> (Float, Float)
		applyOffsets (x, y) (ox, oy) = (x+ox, y+oy)

		V2 x0 y0 = mapTopLeft m
		V2 x1 y1 = mapBottomRight m

mapBottomRight :: Map -> V2 Float 
mapBottomRight Map { mapTopLeft, tileSize, gameMap } =
	mapTopLeft + numTiles * tileSize
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
		bottomRight = filter ((\(x', y') -> (x' + tsx > x) && (y' + tsy > y)) . snd) $ zip coords (tileCoords' m)
		topLeft = filter ((\(x', y') -> (x' < x) && (y' < y)) . snd) bottomRight
		V2 tsx tsy = tileSize m

--test = do
--	let m = Game.mapNew Game.MapConfig { 
--		Game.mapWidth = 9, Game.mapHeight = 9, 
--		Game.mapNeighborhoodFunc = Game.clipNeighborhood
--	}
--	--let rm = newMap m
--	print (tileCoords rm)
--	print $ indexToCoords (Game.mapConfig m) 80

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

newWorldRenderContext gameMap renderMap = do
	--let m = newMap gameMap
	let m = renderMap
	buffers <- GL.genObjectNames 3 :: IO [GL.BufferObject]
	[vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]

	let (Tile tileData) = newTile m
	uploadFromVec GL.ArrayBuffer (buffers!!0) tileData
	--print $ tileData

	--print $ tileColors m

	uploadFromVec GL.ShaderStorageBuffer (buffers!!1) (tileCoords m)
	uploadFromVec GL.ShaderStorageBuffer (buffers!!2) (tileColors m)

	--print m

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

	sampler <- GL.get $ GL.uniformLocation program "Tex1"
	--print sampler
	GL.uniform sampler $= (GL.Index1 (0 :: GL.GLuint))
	--V.unsafeWith workaround $ \ptr -> GLRaw.glUniformMatrix4fv projLoc 1 1 (castPtr ptr)

	GL.drawArraysInstanced GL.Triangles 0 6 (fromIntegral (wrcNumTiles wrc))

