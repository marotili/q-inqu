{-# LANGUAGE NamedFieldPuns #-}
module Game.Render.Map where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Ptr
import System.Exit

import Codec.Picture.Png 
import qualified Codec.Picture as P
import Debug.Trace

import Data.Word
import Data.Maybe
import Linear

import Data.Int
import Data.List
--import Linear.Matrix

import qualified Data.Map as Map
import qualified Data.Vector.Storable as V

import Game.Render.Render

import qualified Data.Tiled as T
import Data.Tiled

data Map = Map
	{ mapTopLeft :: V2 Float
	, tiledMap :: T.TiledMap
	} deriving (Show)

renderMapWidth :: Map -> Int
renderMapWidth m = T.mapWidth . tiledMap $ m

renderMapHeight :: Map -> Int
renderMapHeight m = T.mapHeight . tiledMap $ m

mapTileSize :: Map -> V2 Float
mapTileSize Map { tiledMap } = V2 (fromIntegral . T.mapTileWidth $ tiledMap) (fromIntegral . T.mapTileHeight $ tiledMap)

tilesetTileSize :: T.Tileset -> V2 Float
tilesetTileSize tileSet = V2 (fromIntegral . tsTileWidth $ tileSet) (fromIntegral . T.tsTileHeight $ tileSet)

newRenderMap :: T.TiledMap -> Map
newRenderMap tiledMap = Map
	{ mapTopLeft = V2 0 0
	--, tileSize = V2 tileSizeX tileSizeY
	, tiledMap = tiledMap
	}

data TileMesh = TileMesh (V.Vector Float)

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

newTile :: T.Tileset -> TileMesh
newTile ts = TileMesh $ V.fromList
	[ 0, 0
	, dx, 0
	, dx, dy
	, dx, dy
	, 0, dy
	, 0, 0
	]
	where
		V2 dx dy = tilesetTileSize ts

mapTilesetByGid :: Map -> Int -> Maybe T.Tileset
mapTilesetByGid Map { tiledMap } gid = case find (
		\ts -> fromIntegral (tsInitialGid ts) <= gid && 
			fromIntegral (tsInitialGid ts) + numX ts * numY ts > gid) tilesets of
				Just ts -> Just ts
				Nothing -> Nothing
	where
		tilesets = mapTilesets tiledMap
		numX ts = fromIntegral $ (iWidth . head . tsImages $ ts) `div` tsTileWidth ts
		numY ts = fromIntegral $ (iHeight . head . tsImages $ ts) `div` tsTileHeight ts

tileCoords :: Map -> Layer -> V.Vector Float
tileCoords m ol@ObjectLayer {} = objectCoords m ol
tileCoords m l = V.fromList $ foldr (\(a, b) l -> a : b : l) [] (tileCoords' m l)

tileCoords' :: Map -> Layer -> [(Float, Float)]
--tileCoords' m ol@ObjectLayer { } = objectCoords m ol
tileCoords' m Layer { layerData } = toWorldCoords 
	where
		coords = map (\((x, y), _) -> (fromIntegral x, fromIntegral y)) (Map.toList layerData)
		V2 tlx tly = mapTopLeft m
		(V2 tsx tsy) = mapTileSize m
		toWorldCoords = map (\(lx, ly) -> (tsx * (tlx + lx), tsy * (tly + ly))) coords 

objectCoords :: Map -> Layer -> V.Vector Float
objectCoords m ObjectLayer { layerObjects } = V.fromList $ 
		concatMap (\o -> case ow o of
				Just objectW ->
					[lx + fromIntegral (-objectW + T.objectX o), 
					ly + fromIntegral (-fromJust (T.objectHeight o) + T.objectY o)]
				Nothing -> case objectGid o of
					Just gid -> case mapTilesetByGid m (fromIntegral gid) of
						Just tileset -> 
							[ lx + fromIntegral (-(tsTileWidth tileset) + T.objectX o)
							, ly + fromIntegral (-(tsTileHeight tileset) + T.objectY o)]
						Nothing -> [0, 0]
					Nothing -> [0, 0]
				) layerObjects
	where
		V2 lx ly = mapTopLeft m
		ow = T.objectWidth
objectCoords _ _ = V.fromList []

mapBottomRight :: Map -> V2 Float 
mapBottomRight m@Map { mapTopLeft } =
	mapTopLeft + numTiles * mapTileSize m
	where numTiles = V2 (fromIntegral $ renderMapWidth m) (fromIntegral $ renderMapHeight m)

mapSize :: Map -> V2 Float
mapSize m = mapBottomRight m - mapTopLeft m

-- input in render world coordinates
--mapSelectTile :: V2 Float -> Map -> Maybe (Int, Int)
--mapSelectTile (V2 x y) m = case length topLeft of
--	0 -> Nothing
--	_ -> Just $ fst . head $ topLeft
--	where
--		coords = [(x, y) | 
--			y <- [0, 1.. (Game.mapNumCellsHeight . gameMap $ m)-1],
--			x <- [0, 1.. (Game.mapNumCellsWidth . gameMap $ m)-1]
--			]
--		bottomRight = filter ((\(x', y') -> (x' + tsx > x) && (y' + tsy > y)) . snd) $ zip coords (tileCoords' m)
--		topLeft = filter ((\(x', y') -> (x' < x) && (y' < y)) . snd) bottomRight
--		V2 tsx tsy = tileSize m

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
	, wrcTileBuffers :: [GL.BufferObject]
	, wrcPosSSBs :: [GL.BufferObject]
	, wrcTilesetSSB :: GL.BufferObject
	, wrcLayerSSBs :: [GL.BufferObject]
	, wrcObjectSSBs :: [GL.BufferObject]
	, wrcTextures :: [GL.TextureObject]
	, wrcNumTiles :: Int
	, wrcMap :: Map
	}

objectLayers :: Map -> [Layer]
objectLayers Map { tiledMap } = filter (\l -> case l of T.ObjectLayer {} -> True; _ -> False) (T.mapLayers tiledMap)

layers :: Map -> [Layer]
layers Map { tiledMap } = T.mapLayers tiledMap

numLayers :: Map -> Int
numLayers map = length . layers $ map
numObjectLayers :: Map -> Int
numObjectLayers map = length . objectLayers $ map

tileIds :: Layer -> V.Vector Int32
tileIds Layer { layerData } = V.fromList $ map (fromIntegral . T.tileGid . snd) (Map.toList layerData)
tileIds ObjectLayer { layerObjects } = V.fromList $ map (fromIntegral . fromJust . T.objectGid) layerObjects
tileIds _ = V.fromList []

images :: Map -> [T.Image]
images Map { tiledMap} = concatMap T.tsImages (T.mapTilesets tiledMap)

numImages :: Map -> Int
numImages map = length . images $ map

numObjects :: Layer -> Int
numObjects ObjectLayer { layerObjects } = length layerObjects
numObjects _ = 0

tileSetData :: Map -> V.Vector Int32
tileSetData Map { tiledMap } = V.fromList . map fromIntegral $ 
		concatMap (\(ts, i) -> map ($ts) (getters i)) (zip (mapTilesets tiledMap) [0..])
	where
		getters :: Int -> [Data.Tiled.Tileset -> Int]
		getters i = 
			[ fromIntegral . tsInitialGid
			, iWidth . head . tsImages
			, iHeight . head . tsImages
			, tsSpacing
			, tsMargin
			, tsTileWidth
			, tsTileHeight
			, const i
			]

--meshSizes = 
 
newWorldRenderContext :: Map -> IO WorldRenderContext
newWorldRenderContext renderMap = do
	--let m = newMap gameMap
	tileBuffers <- GL.genObjectNames (length $ mapTilesets (tiledMap renderMap)) :: IO [GL.BufferObject]
	posBuffers <- GL.genObjectNames (numLayers renderMap) :: IO [GL.BufferObject]
	layerBuffers <- GL.genObjectNames (numLayers renderMap) :: IO [GL.BufferObject]
	objectBuffers <- GL.genObjectNames (numObjectLayers renderMap) :: IO [GL.BufferObject]
	[tilesetBuffer] <- GL.genObjectNames 1 :: IO [GL.BufferObject]
	[vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]

	imageTextures <- GL.genObjectNames (numImages renderMap) :: IO [GL.TextureObject]
	--print $ "Num images" ++ show (numImages renderMap)
	--print $ concatMap tsImages (mapTilesets (tiledMap renderMap))

	-- global data
	mapM_ (\(tileBuffer, tileset) -> do
			let (TileMesh tileData) = newTile tileset
			uploadFromVec GL.ShaderStorageBuffer tileBuffer tileData
		) $ zip tileBuffers (mapTilesets . tiledMap $ renderMap)

	-- tileset infos
	uploadFromVec GL.ShaderStorageBuffer tilesetBuffer (tileSetData renderMap)
	--print $ tileSetData renderMap

	-- per map buffers
	--print $ ("tileids" ++ show (V.length (tileCoords renderMap)))
	--print $ "Num layers" ++ show  (length (T.mapLayers . tiledMap $ renderMap))

	-- per layer buffer
	-- tile types
	mapM_ (\(layerBuffer, posBuffer, layer) -> do
			uploadFromVec GL.ShaderStorageBuffer layerBuffer (tileIds layer)
			uploadFromVec GL.ShaderStorageBuffer posBuffer (tileCoords renderMap layer)
			--print $ ("tileids" ++ show (tileCoords renderMap layer))
			--print $ ("tileids" ++ show (objectCoords renderMap layer))
		) $ zip3 layerBuffers posBuffers (T.mapLayers . tiledMap $ renderMap)
	--print $ "Num layers" ++ show length (T.mapLayers . tiledMap $ renderMap)

	-- object coordinates
	--mapM_ (\(posBuffer, layer) ->
			--uploadFromVec GL.ShaderStorageBuffer posBuffer (objectCoords renderMap layer)
		--) $ zip objectBuffers (objectLayers renderMap)

	-- per image stuff
	mapM_ (\(i, texObject, image) -> do
		GL.activeTexture $= GL.TextureUnit i

		-- Make it the "currently bound 2D texture"
		GL.textureBinding GL.Texture2D $= Just texObject

		pngImage <- P.readPng (T.iSource image)
		case pngImage of
			Left s -> return ()
			Right s -> case s of
				(P.ImageRGBA8 (P.Image imgWidth imgHeight dat)) ->
					--if (imgWidth /= T.iWidth image || imgHeight /= T.iHeight image)
						--then print $ "Image dimensions dont match: " ++ show (T.iSource image)
						--else return ()
					V.unsafeWith dat $ \ptr -> do
						GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 
							(GL.TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0 
							(GL.PixelData GL.RGBA GL.UnsignedByte ptr)
						GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
		) $ zip3 [0..] imageTextures (images renderMap)

	--print m

	return WorldRenderContext
		{ wrcVao = vao
		, wrcTileBuffers = tileBuffers
		, wrcPosSSBs = posBuffers
		, wrcLayerSSBs = layerBuffers
		, wrcObjectSSBs = objectBuffers
		, wrcTextures = imageTextures
		, wrcNumTiles = renderMapWidth renderMap * renderMapHeight renderMap
		, wrcMap = renderMap
		, wrcTilesetSSB = tilesetBuffer
		}

bindWorldRenderContext :: WorldRenderContext -> GL.Program -> IO ()
bindWorldRenderContext wrc program = 
	GL.bindVertexArrayObject $= (Just $ wrcVao wrc)	
	--mesh <- GL.get (GL.attribLocation program "mesh")

 --	GL.vertexAttribArray mesh $= GL.Enabled
 --	GL.bindBuffer GL.ArrayBuffer $= Just (wrcMeshBuffer wrc)
 --	GL.vertexAttribPointer mesh $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)

renderNormalLayer :: GL.Program -> WorldRenderContext -> GL.BufferObject -> GL.BufferObject -> IO ()
renderNormalLayer program wrc layerSSB posSSB = do
	posIndex <- GL.getShaderStorageBlockIndex program "Pos"
	layerIndex <- GL.getShaderStorageBlockIndex program "LayerData"

	renderType <- GL.get $ GL.uniformLocation program "renderType"
	GL.uniform renderType $= GL.Index1 (0 :: GL.GLuint)

	GL.bindBufferBase' GL.ShaderStorageBuffer posIndex posSSB
	GL.shaderStorageBlockBinding program posIndex posIndex	

	GL.bindBufferBase' GL.ShaderStorageBuffer layerIndex layerSSB
	GL.shaderStorageBlockBinding program layerIndex layerIndex

	GL.drawArraysInstanced GL.Triangles 0 6 (fromIntegral (wrcNumTiles wrc))

--renderObjectLayer program wrc objectLayer = do
--	posIndex <- GL.getShaderStorageBlockIndex program "Pos"
--	objectIndex <- GL.getShaderStorageBlockIndex program "ObjectData"

--	let (Just index) = elemIndex objectLayer (objectLayers . wrcMap $ wrc)
--	let objectSSB = (wrcObjectSSBs wrc !! index)

--	renderType <- GL.get $ GL.uniformLocation program "renderType"
--	GL.uniform renderType $= (GL.Index1 (1 :: GL.GLuint))

--	GL.bindBufferBase' GL.ShaderStorageBuffer posIndex objectSSB
--	GL.shaderStorageBlockBinding program posIndex posIndex	

--	GL.drawArraysInstanced GL.Triangles 0 6 (fromIntegral (numObjects objectLayer))

renderWorldRenderContext :: GL.Program -> WorldRenderContext -> IO ()
renderWorldRenderContext program wrc = do
	let map = wrcMap wrc

	tilesetIndex <- GL.getShaderStorageBlockIndex program "TileSets"
	GL.bindBufferBase' GL.ShaderStorageBuffer tilesetIndex (wrcTilesetSSB wrc)
	GL.shaderStorageBlockBinding program tilesetIndex tilesetIndex	

	meshIndex <- GL.getShaderStorageBlockIndex program "Mesh"
	GL.bindBufferBase' GL.ShaderStorageBuffer meshIndex (head . wrcTileBuffers $ wrc)
	GL.shaderStorageBlockBinding program meshIndex meshIndex	

	errors <- GL.get GL.errors
	--print $ ("sampler", errors)
	mapM_ (\i -> do
			sampler <- GL.get $ GL.uniformLocation program ("Texture" ++ show i)
			--errors <- GL.get GL.errors
			--print $ ("uniformloc", errors, sampler)
			GL.uniform sampler $= GL.TextureUnit (fromIntegral i)
			--errors <- GL.get GL.errors
			--print $ ("setuniform", errors)
		) [0..length (wrcTextures wrc)-1]
	errors <- GL.get GL.errors
	--print $ ("render", errors)
	mapM_ (\(layerSSB, posSSB, layer) ->
			renderNormalLayer program wrc layerSSB posSSB
		) $ zip3 (wrcLayerSSBs wrc) (wrcPosSSBs wrc) (layers map)
