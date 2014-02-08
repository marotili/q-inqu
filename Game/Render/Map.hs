{-# LANGUAGE NamedFieldPuns, TemplateHaskell #-}
module Game.Render.Map 
	(
	  WorldRenderContext
	, newWorldRenderContext
	, bindWorldRenderContext
	, updateWorldRenderContext
	, renderWorldRenderContext

	-- * To remove
	, Map(..)
	, wrcMap, tiledMap
	, newRenderMap
	) where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW
import Foreign.Ptr
import System.Exit
import Game.Render.Error
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
		numX ts = fromIntegral $ fromJust (ts^.tsImages^?_head.iWidth) `div` (ts^.tsTileWidth)
		numY ts = fromIntegral $ fromJust (ts^.tsImages^?_head.iHeight) `div` (ts^.tsTileHeight)

tileCoords :: Map -> Layer -> V.Vector Float
tileCoords m ol@ObjectLayer {} = objectCoords m ol
tileCoords m l = V.fromList $ foldr (\(a, b) l -> a : b : p : p : l) [] (tileCoords' m l)
	where p = 0.0 -- padding

tileCoords' :: Map -> Layer -> [(Float, Float)]
--tileCoords' m ol@ObjectLayer { } = objectCoords m ol
tileCoords' m Layer { _layerData } = toWorldCoords 
	where
		coords = map (\((x, y), _) -> (fromIntegral x, -fromIntegral y)) (Map.toList _layerData)
		V2 tlx tly = m^.mapTopLeft
		(V2 tsx tsy) = mapTileSize m
		toWorldCoords = map (\(lx, ly) -> (tsx * (tlx + lx), tsy * (tly + ly))) coords 

objectCoords :: Map -> Layer -> V.Vector Float
objectCoords m ObjectLayer { _layerObjects } = V.fromList $ 
		concatMap (\o -> case ow o of
				Just objectW ->
					[lx + fromIntegral (o^.objectX), 
					-(ly + fromIntegral (-fromJust (o^.objectHeight) + (o^.objectY)))
					, p, p]				
				Nothing -> case o^.objectGid of
					Just gid -> case mapTilesetByGid m (fromIntegral gid) of
						Just tileset -> 
							[ lx + fromIntegral (o^.objectX)
							, -(ly + fromIntegral (-(tileset^.tsTileHeight) + (o^.objectY)))
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

updateWorldRenderContext :: WorldRenderContext -> IO ()
updateWorldRenderContext wrc =
	mapM_ (\(layerBuffer, posBuffer, layer) ->
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
	logGL "newWorldRenderContext: genObjects"

	uploadFromVec GL.UniformBuffer tilesetBuffer (tileSetData renderMap)

	-- per layer buffer
	-- tile types
	mapM_ (\(layerBuffer, posBuffer, layer) -> do
			uploadFromVec GL.UniformBuffer layerBuffer (tileIds layer)
			uploadFromVec GL.UniformBuffer posBuffer (tileCoords renderMap layer)
			print $ tileCoords renderMap layer
			case layer of
				ObjectLayer {} ->
					print $ "Objectdata" ++ show (tileIds layer) ++ " / " ++ show (tileCoords renderMap layer)
				_ -> return ()
		) $ zip3 layerBuffers posBuffers (renderMap^.tiledMap.mapLayers)

	-- per image stuff
	mapM_ (\(i, texObject, image) -> do
		GL.activeTexture $= GL.TextureUnit i
		logGL "newWorldRenderContext: activeTexture"

		-- Make it the "currently bound 2D texture"
		GL.textureBinding GL.Texture2D $= Just texObject
		logGL "newWorldRenderContext: textureBinding"

		pngImage <- P.readPng (image^.iSource)
		case pngImage of
			Left s -> return ()
			Right s -> case s of
				(P.ImageRGBA8 (P.Image imgWidth imgHeight dat)) ->
					V.unsafeWith dat $ \ptr -> do
						GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 
							(GL.TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0 
							(GL.PixelData GL.RGBA GL.UnsignedByte ptr)
						logGL "newWorldRenderContext: texImage2D"
						GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
						logGL "newWorldRenderContext: textureFilter"
		) $ zip3 [0..] imageTextures (images renderMap)

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

renderNormalLayer :: GL.Program -> WorldRenderContext -> GL.BufferObject -> GL.BufferObject -> Layer -> IO ()
renderNormalLayer program wrc layerSSB posSSB layer = do
	posIndex <- GL.getUniformBlockIndex program "Pos"
	layerIndex <- GL.getUniformBlockIndex program "LayerData"

	GL.bindBufferBase' GL.UniformBuffer posIndex posSSB
	logGL "renderNormalLayer: bindBufferBase' posIndex"
	GL.uniformBlockBinding program posIndex posIndex	
	logGL "renderNormalLayer: uniformBlockBinding posIndex"

	GL.bindBufferBase' GL.UniformBuffer layerIndex layerSSB
	logGL "renderNormalLayer: bindBufferBase' layerIndex"
	GL.uniformBlockBinding program layerIndex layerIndex
	logGL "renderNormalLayer: uniformBlockBinding layerIndex"

	GL.drawArraysInstanced GL.Triangles 0 6 (fromIntegral (numObjects layer))
	logGL "renderNormalLayer: drawArraysInstanced"

renderWorldRenderContext :: GL.Program -> WorldRenderContext -> IO ()
renderWorldRenderContext program wrc = do
	let map = wrc^.wrcMap

	numTilesets <- GL.get $ GL.uniformLocation program "numTileSets"
	logGL "renderWorldRenderContext: uniformLoc numTilesets"
	GL.uniform numTilesets $= GL.Index1 (fromIntegral . length $ map^.tiledMap^.mapTilesets :: GL.GLint)
	logGL "renderWorldRenderContext: uniform numTilesets"

	tilesetIndex <- GL.getUniformBlockIndex program "TileSets"
	logGL "renderWorldRenderContext: getUniformBlockIndex"
	GL.bindBufferBase' GL.UniformBuffer tilesetIndex (wrc^.wrcTilesetSSB)
	logGL "renderWorldRenderContext: bindBufferBase' tilesetIndex"
	GL.uniformBlockBinding program tilesetIndex tilesetIndex	
	logGL "renderWorldRenderContext: uniformBlockBinding tilesetIndex"

	mapM_ (\i -> do
			sampler <- GL.get $ GL.uniformLocation program ("Texture" ++ show i)
			logGL "renderWorldRenderContext: uniformLoc sampler"
			GL.uniform sampler $= GL.TextureUnit (fromIntegral i)
			logGL "renderWorldRenderContext: uniform sampler"
		) [0..length (wrc^.wrcTextures)-1]

	mapM_ (\(layerSSB, posSSB, layer) ->
			renderNormalLayer program wrc layerSSB posSSB layer
		) $ zip3 (wrc^.wrcLayerSSBs) (wrc^.wrcPosSSBs) (layers map)
