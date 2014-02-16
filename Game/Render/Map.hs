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
import Game.Render.Error
import qualified Codec.Picture as P

import Data.Maybe
import Linear

import Data.Int

import qualified Data.Map as Map
import qualified Data.Vector.Storable as V

import Game.Render.Render

import qualified Data.Tiled as T
import Data.Tiled
import Game.World.Import.Tiled

import Control.Lens

data Map = Map
	{ _mapTopLeft :: V2 Float
	, _tiledMap :: T.TiledMap
	} deriving (Show)
makeLenses ''Map

newRenderMap :: T.TiledMap -> Map
newRenderMap tMap = Map
	{ _mapTopLeft = V2 0 0
	, _tiledMap = tMap
	}

tileCoords :: Map -> Layer -> V.Vector Float
tileCoords m ol@ObjectLayer {} = objectCoords m ol
tileCoords m layer = V.fromList $ foldr (\(a, b) l -> a : b : p : p : l) [] (tileCoords' m layer)
	where p = 0.0 -- padding

tileCoords' :: Map -> Layer -> [(Float, Float)]
tileCoords' m Layer { _layerData } = toWorldCoords 
	where
		coords = map (\((x, y), _) -> (fromIntegral x, -fromIntegral y)) (Map.toList _layerData)
		V2 tlx tly = m^.mapTopLeft
		(tsx, tsy) = m^.tiledMap.mapTileSize
		toWorldCoords = map (\(lx, ly) -> (tlx + tsx * lx , tly + tsy * ly)) coords 

tileCoords' _ _ = []

objectCoords :: Map -> Layer -> V.Vector Float
objectCoords m ObjectLayer { _layerObjects } = V.fromList $ 
		concatMap (\o -> let (ox, oy) = o^.objectPos (m^.tiledMap) in
				[lx + ox, -(ly + oy), 0, 0]
				) _layerObjects
	where
		V2 lx ly = m^.mapTopLeft
objectCoords _ _ = V.fromList []

data WorldRenderContext = WorldRenderContext
	{ _wrcVao :: GL.VertexArrayObject
	, _wrcPosSSBs :: [GL.BufferObject]
	, _wrcTilesetSSB :: GL.BufferObject
	, _wrcLayerSSBs :: [GL.BufferObject]
	, _wrcTextures :: [GL.TextureObject]
	, _wrcMap :: Map
	}

makeLenses ''WorldRenderContext

layers :: Map -> [Layer]
layers Map { _tiledMap } = _tiledMap^.mapLayers

numLayers :: Map -> Int
numLayers worldMap = length . layers $ worldMap

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
numImages worldMap = length . images $ worldMap

numObjects :: Layer -> Int
numObjects ObjectLayer { _layerObjects } = length _layerObjects
numObjects Layer { _layerData } = Map.size _layerData
numObjects _ = 0

tileSetData :: Map -> V.Vector Int32
tileSetData Map { _tiledMap } = V.fromList . map fromIntegral $ 
		concatMap (\(tileset, i) -> map (\f -> f tileset) (getters i)) (zip (_tiledMap^.mapTilesets) [0..])
		++ padding
	where
		padding = concat [[0, 0, 0, 0,
							 0, 0, 0, 0,
							 0, 0, 0, 0,
							 0, 0, 0, 0] | _ <- [0..10 - length (_tiledMap^.mapTilesets)- 1]]
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
			] ++ fmap const [0, 0, 0, 0, 0, 0, 0, 0]

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
	posBuffers <- GL.genObjectNames (numLayers renderMap) :: IO [GL.BufferObject]
	layerBuffers <- GL.genObjectNames (numLayers renderMap) :: IO [GL.BufferObject]
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
			Left _ -> return ()
			Right s -> case s of
				(P.ImageRGBA8 (P.Image imgWidth imgHeight dat)) ->
					V.unsafeWith dat $ \ptr -> do
						GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 
							(GL.TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0 
							(GL.PixelData GL.RGBA GL.UnsignedByte ptr)
						logGL "newWorldRenderContext: texImage2D"
						GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
						logGL "newWorldRenderContext: textureFilter"
				_ -> error "Only RGBA8 supported"
		) $ zip3 [0..] imageTextures (images renderMap)

	return WorldRenderContext
		{ _wrcVao = vao
		--, _wrcTileBuffers = tileBuffers
		, _wrcPosSSBs = posBuffers
		, _wrcLayerSSBs = layerBuffers
		--, _wrcObjectSSBs = objectBuffers
		, _wrcTextures = imageTextures
		--, _wrcNumTiles = renderMapWidth renderMap * renderMapHeight renderMap
		, _wrcMap = renderMap
		, _wrcTilesetSSB = tilesetBuffer
		}

bindWorldRenderContext :: WorldRenderContext -> GL.Program -> IO ()
bindWorldRenderContext wrc _ = 
	GL.bindVertexArrayObject $= (Just $ wrc^.wrcVao)	

renderNormalLayer :: GL.Program -> GL.BufferObject -> GL.BufferObject -> Layer -> IO ()
renderNormalLayer program layerSSB posSSB layer = do
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
	let worldMap = wrc^.wrcMap
	bindWorldRenderContext wrc program

	numTilesets <- GL.get $ GL.uniformLocation program "numTileSets"
	logGL "renderWorldRenderContext: uniformLoc numTilesets"
	GL.uniform numTilesets $= GL.Index1 (fromIntegral . length $ worldMap^.tiledMap^.mapTilesets :: GL.GLint)
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
			renderNormalLayer program layerSSB posSSB layer
		) $ zip3 (wrc^.wrcLayerSSBs) (wrc^.wrcPosSSBs) (layers worldMap)
