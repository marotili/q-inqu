{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}
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

import Control.Lens

data Map = Map
	{ _mapTopLeft :: V2 Float
	, _tiledMap :: T.TiledMap
	} deriving (Show)
makeLenses ''Map

renderMapWidth :: Map -> Int
renderMapWidth m = m^.tiledMap.mapWidth

renderMapHeight :: Map -> Int
renderMapHeight m = m^.tiledMap.mapHeight

mapTileSize :: Map -> V2 Float
mapTileSize Map { _tiledMap } = V2 (fromIntegral $ tiledMap^.mapTileWidth) (fromIntegral $ tiledMap^.mapTileHeight)
	where
		tiledMap = _tiledMap

tilesetTileSize :: T.Tileset -> V2 Float
tilesetTileSize tileSet = V2 (fromIntegral $ tileSet^.tsTileWidth) (fromIntegral $ tileSet^.tsTileHeight)

newRenderMap :: T.TiledMap -> Map
newRenderMap tiledMap = Map
	{ _mapTopLeft = V2 0 0
	--, tileSize = V2 tileSizeX tileSizeY
	, _tiledMap = tiledMap
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
mapTilesetByGid Map { _tiledMap } gid = case find (
		\ts -> fromIntegral (ts^.tsInitialGid) <= gid && 
			fromIntegral (ts^.tsInitialGid) + numX ts * numY ts > gid) tilesets of
				Just ts -> Just ts
				Nothing -> Nothing
	where
		tiledMap = _tiledMap
		tilesets = tiledMap^.mapTilesets
		numX ts = fromIntegral $ (fromJust $ ts^.tsImages^?_head.iWidth) `div` (ts^.tsTileWidth)
		numY ts = fromIntegral $ (fromJust $ ts^.tsImages^?_head.iHeight) `div` (ts^.tsTileHeight)

tileCoords :: Map -> Layer -> V.Vector Float
tileCoords m ol@ObjectLayer {} = objectCoords m ol
tileCoords m l = V.fromList $ foldr (\(a, b) l -> a : b : p : p : l) [] (tileCoords' m l)
	where p = 0.0 -- padding

tileCoords' :: Map -> Layer -> [(Float, Float)]
--tileCoords' m ol@ObjectLayer { } = objectCoords m ol
tileCoords' m Layer { _layerData } = toWorldCoords 
	where
		coords = map (\((x, y), _) -> (fromIntegral x, fromIntegral y)) (Map.toList _layerData)
		V2 tlx tly = m^.mapTopLeft
		(V2 tsx tsy) = mapTileSize m
		toWorldCoords = map (\(lx, ly) -> (tsx * (tlx + lx), tsy * (tly + ly))) coords 

objectCoords :: Map -> Layer -> V.Vector Float
objectCoords m ObjectLayer { _layerObjects } = V.fromList $ 
		concatMap (\o -> case ow o of
				Just objectW ->
					[lx + fromIntegral (o^.objectX), 
					ly + fromIntegral (-fromJust (o^.objectHeight) + (o^.objectY))
					, p, p]				
				Nothing -> case o^.objectGid of
					Just gid -> case mapTilesetByGid m (fromIntegral gid) of
						Just tileset -> 
							[ lx + fromIntegral ((o^.objectX))
							, ly + fromIntegral (-(tileset^.tsTileHeight) + (o^.objectY))
							, p, p
							]
						Nothing -> [0, 0, 0, 0]
					Nothing -> [0, 0, 0, 0]
				) _layerObjects
	where
		V2 lx ly = m^.mapTopLeft
		ow o = o^.objectWidth
		p = 0.0 -- padding
objectCoords _ _ = V.fromList []

mapBottomRight :: Map -> V2 Float 
mapBottomRight m@Map { _mapTopLeft } =
	_mapTopLeft + numTiles * mapTileSize m
	where numTiles = V2 (fromIntegral $ renderMapWidth m) (fromIntegral $ renderMapHeight m)

mapSize :: Map -> V2 Float
mapSize m = mapBottomRight m - _mapTopLeft m

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
	{ _wrcVao :: GL.VertexArrayObject
	--, wrcMeshBuffer :: V.Vector Float
	, _wrcTileBuffers :: [GL.BufferObject]
	, _wrcPosSSBs :: [GL.BufferObject]
	, _wrcTilesetSSB :: GL.BufferObject
	, _wrcLayerSSBs :: [GL.BufferObject]
	, _wrcObjectSSBs :: [GL.BufferObject]
	, _wrcTextures :: [GL.TextureObject]
	, _wrcNumTiles :: Int
	, _wrcMap :: Map
	}

makeLenses ''WorldRenderContext

objectLayers :: Map -> [Layer]
objectLayers Map { _tiledMap } = 
	filter (\l -> case l of T.ObjectLayer {} -> True; _ -> False) (_tiledMap^.mapLayers)

layers :: Map -> [Layer]
layers Map { _tiledMap } = _tiledMap^.mapLayers

numLayers :: Map -> Int
numLayers map = length . layers $ map
numObjectLayers :: Map -> Int
numObjectLayers map = length . objectLayers $ map

tileIds :: Layer -> V.Vector Int32
tileIds Layer { _layerData } = V.fromList $ concatMap (\x -> [x, p, p, p]) $ 
		map (fromIntegral . _tileGid . snd) (Map.toList _layerData)
	where p = 0 :: Int32 -- padding
tileIds ObjectLayer { _layerObjects } = V.fromList $ concatMap (\x -> [x, p, p, p]) $ 
		map (fromIntegral . fromJust . _objectGid) _layerObjects
	where p = 0 :: Int32 -- padding
tileIds _ = V.fromList []

images :: Map -> [T.Image]
images Map { _tiledMap} = concatMap _tsImages (_tiledMap^.mapTilesets)

numImages :: Map -> Int
numImages map = length . images $ map

numObjects :: Layer -> Int
numObjects ObjectLayer { _layerObjects } = length _layerObjects
numObjects Layer { _layerData } = length $ Map.toList _layerData

tileSetData :: Map -> V.Vector Int32
tileSetData Map { _tiledMap } = V.fromList . map fromIntegral $ 
		concatMap (\(tileset, i) -> map (\f -> f tileset) (getters i)) (zip (_tiledMap^.mapTilesets) [0..])
	where
		getters :: Int -> [Data.Tiled.Tileset -> Int]
		getters i = 
			[ fromIntegral . _tsInitialGid
			, _iWidth . head . _tsImages
			, _iHeight . head . _tsImages
			, _tsSpacing
			, _tsMargin
			, _tsTileWidth
			, _tsTileHeight
			, const i
			]

--meshSizes = 

--updateWorldRenderContext :: Map -> WorldRenderContext -> WorldRenderContext
updateWorldRenderContext wrc = do
	mapM_ (\(layerBuffer, posBuffer, layer) -> do
			case layer of
				ObjectLayer {} -> do
					updateFromVec GL.UniformBuffer layerBuffer (tileIds layer)
					updateFromVec GL.UniformBuffer posBuffer (tileCoords (wrc^.wrcMap) layer)
				_ -> return ()
		) $ zip3 (wrc^.wrcLayerSSBs) (wrc^.wrcPosSSBs) (wrc^.wrcMap.tiledMap.mapLayers)
 
newWorldRenderContext :: Map -> IO WorldRenderContext
newWorldRenderContext renderMap = do
	--let m = newMap gameMap
	tileBuffers <- GL.genObjectNames (length $ renderMap^.tiledMap.mapTilesets) :: IO [GL.BufferObject]
	posBuffers <- GL.genObjectNames (numLayers renderMap) :: IO [GL.BufferObject]
	layerBuffers <- GL.genObjectNames (numLayers renderMap) :: IO [GL.BufferObject]
	objectBuffers <- GL.genObjectNames (numObjectLayers renderMap) :: IO [GL.BufferObject]
	[tilesetBuffer] <- GL.genObjectNames 1 :: IO [GL.BufferObject]
	[vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]

	imageTextures <- GL.genObjectNames (numImages renderMap) :: IO [GL.TextureObject]
	--print $ "Num images" ++ show (numImages renderMap)
	--print $ concatMap tsImages (mapTilesets (tiledMap renderMap))

	-- global data
--	mapM_ (\(tileBuffer, tileset) -> do
--			let (TileMesh tileData) = newTile tileset
--			uploadFromVec GL.UniformBuffer tileBuffer tileData
--		) $ zip tileBuffers (renderMap^.tiledMap.mapTilesets)

	-- tileset infos
	uploadFromVec GL.UniformBuffer tilesetBuffer (tileSetData renderMap)
	--print $ tileSetData renderMap
	-- per map buffers
	--print $ ("tileids" ++ show (V.length (tileCoords renderMap)))
	--print $ "Num layers" ++ show  (length (T.mapLayers . tiledMap $ renderMap))

	-- per layer buffer
	-- tile types
	mapM_ (\(layerBuffer, posBuffer, layer) -> do
			uploadFromVec GL.UniformBuffer layerBuffer (tileIds layer)
			uploadFromVec GL.UniformBuffer posBuffer (tileCoords renderMap layer)
			case layer of
				ObjectLayer {} ->
					print $ "Objectdata" ++ show (tileIds layer) ++ " / " ++ show (tileCoords renderMap layer)
				_ -> return ()
			--print $ ("tileids" ++ show (tileCoords renderMap layer))
			--print $ ("tileids" ++ show (objectCoords renderMap layer))
		) $ zip3 layerBuffers posBuffers (renderMap^.tiledMap.mapLayers)
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

		pngImage <- P.readPng (image^.iSource)
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
		{ _wrcVao = vao
		, _wrcTileBuffers = tileBuffers
		, _wrcPosSSBs = posBuffers
		, _wrcLayerSSBs = layerBuffers
		, _wrcObjectSSBs = objectBuffers
		, _wrcTextures = imageTextures
		, _wrcNumTiles = renderMapWidth renderMap * renderMapHeight renderMap
		, _wrcMap = renderMap
		, _wrcTilesetSSB = tilesetBuffer
		}

bindWorldRenderContext :: WorldRenderContext -> GL.Program -> IO ()
bindWorldRenderContext wrc program = 
	GL.bindVertexArrayObject $= (Just $ wrc^.wrcVao)	
	--mesh <- GL.get (GL.attribLocation program "mesh")

 --	GL.vertexAttribArray mesh $= GL.Enabled
 --	GL.bindBuffer GL.ArrayBuffer $= Just (wrcMeshBuffer wrc)
 --	GL.vertexAttribPointer mesh $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 nullPtr)

renderNormalLayer :: GL.Program -> WorldRenderContext -> GL.BufferObject -> GL.BufferObject -> Layer -> IO ()
renderNormalLayer program wrc layerSSB posSSB layer = do
	posIndex <- GL.getUniformBlockIndex program "Pos"
	layerIndex <- GL.getUniformBlockIndex program "LayerData"

	--renderType <- GL.get $ GL.uniformLocation program "renderType"

	GL.bindBufferBase' GL.UniformBuffer posIndex posSSB
	GL.uniformBlockBinding program posIndex posIndex	

	GL.bindBufferBase' GL.UniformBuffer layerIndex layerSSB
	GL.uniformBlockBinding program layerIndex layerIndex

	GL.drawArraysInstanced GL.Triangles 0 6 (fromIntegral (numObjects layer))

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
	let map = wrc^.wrcMap

	numTilesets <- GL.get $ GL.uniformLocation program ("numTileSets")
	GL.uniform numTilesets $= GL.Index1 (fromIntegral . length $ map^.tiledMap^.mapTilesets :: GL.GLint)

	tilesetIndex <- GL.getUniformBlockIndex program "TileSets"
	GL.bindBufferBase' GL.UniformBuffer tilesetIndex (wrc^.wrcTilesetSSB)
	GL.uniformBlockBinding program tilesetIndex tilesetIndex	

	--meshIndex <- GL.getUniformBlockIndex program "Mesh"
	--GL.bindBufferBase' GL.UniformBuffer meshIndex (head . wrcTileBuffers $ wrc)
	--GL.uniformBlockBinding program meshIndex meshIndex	

	mapM_ (\i -> do
			sampler <- GL.get $ GL.uniformLocation program ("Texture" ++ show i)
			--errors <- GL.get GL.errors
			--print $ ("uniformloc", errors, sampler)
			GL.uniform sampler $= GL.TextureUnit (fromIntegral i)
			--errors <- GL.get GL.errors
			--print $ ("setuniform", errors)
		) [0..length (wrc^.wrcTextures)-1]
	errors <- GL.get GL.errors

	--print $ ("render", errors)
	mapM_ (\(layerSSB, posSSB, layer) ->
			renderNormalLayer program wrc layerSSB posSSB layer
		) $ zip3 (wrc^.wrcLayerSSBs) (wrc^.wrcPosSSBs) (layers map)
