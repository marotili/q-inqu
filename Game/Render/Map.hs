{-# LANGUAGE NamedFieldPuns, Rank2Types, TemplateHaskell #-}
module Game.Render.Map 
	(
	  WorldRenderContext
	, newWorldRenderContext
	, bindWorldRenderContext
	, updateWorldRenderContext
	, renderWorldRenderContext

	, wrcWorld

	-- * To remove
	) where

import Data.List

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Game.Render.Error
import qualified Codec.Picture as P

import Data.Maybe
import Linear

import Data.Int

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V

import Game.Render.Render

import qualified Data.Tiled as T
import Data.Tiled
import Game.World.Import.Tiled

import Control.Lens
import qualified Game.Render.World as R


data WorldRenderContext = WorldRenderContext
	{ _wrcVao :: GL.VertexArrayObject

	, _wrcPosSSBs :: Map.Map R.LayerId GL.BufferObject
	, _wrcLayerSSBs :: Map.Map R.LayerId GL.BufferObject

	, _wrcTextures :: Map.Map R.TilesetId (Int, GL.TextureObject)
	, _wrcTilesetSSB :: GL.BufferObject
	, _wrcWorld :: R.World
	}

makeLenses ''WorldRenderContext
wPosData :: Getter [(Float, Float)] (V.Vector Float)
wPosData = to (\poss -> V.fromList $ concatMap (\(x, y) -> [x, y, 0, 0]) poss)
wTileData :: Getter [Int] (V.Vector Int32)
wTileData = to (\ids -> V.fromList $ concatMap (\tId -> [fromIntegral tId, 0, 0, 0]) ids)


wTilesetData :: WorldRenderContext -> Getter R.World (V.Vector Int32)
wTilesetData wrc = to get
	where
		get world = V.fromList $ map fromIntegral $ concat $
			sortBy tsInitialIdSort $ map(\(i, id, ts) -> tsData i ts id)
				$ zip3 [0..] ids tss
			where
				tsInitialIdSort (tsId:xs) (tsId2:ys) 
					| tsId < tsId2 = LT
					| otherwise = GT
				ids = Map.elems $ world^.R.mapHashes.R.gameTilesets
				tss = map (\tsId -> world^?!R.mapTilesets.at tsId._Just) ids
				tsData img ts tsId = 
					[ world^?!R.mapTsOffsets.at tsId._Just
					, ts^.R.tsImage.R.iWidth
					, ts^.R.tsImage.R.iHeight
					, ts^.R.tsSpacing
					, ts^.R.tsMargin
					, ts^.R.tsTileWidth
					, ts^.R.tsTileHeight
					, fst (wrc^?!wrcTextures.at tsId._Just)
					, 0, 0, 0, 0, 0, 0, 0, 0
					]

updateWorldRenderContext :: WorldRenderContext -> IO ()
updateWorldRenderContext wrc =
	mapM_ (\layerId -> do
		let layerName = wrc^.wrcWorld.R.wLayerName layerId
		let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
		let Just posBuf = wrc^.wrcPosSSBs.at layerId
		print (wrc^.wrcWorld.R.wTileIds layerName.wTileData)
		print (wrc^.wrcWorld.R.wTilePos layerName.wPosData)
		updateFromVec GL.ShaderStorageBuffer layerBuf 
			(wrc^.wrcWorld.R.wTileIds layerName.wTileData)
		updateFromVec GL.ShaderStorageBuffer posBuf 
			(wrc^.wrcWorld.R.wTilePos layerName.wPosData)
		) (Set.toList $ wrc^.wrcWorld.R.mapUpdateLayers)
 
newWorldRenderContext :: R.World -> IO WorldRenderContext
newWorldRenderContext world = do
	posBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]
	layerBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]

	[tilesetBuffer] <- GL.genObjectNames 1 :: IO [GL.BufferObject]
	[vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]

	imageTextures <- GL.genObjectNames (Map.size (world^.R.mapTilesets)) :: IO [GL.TextureObject]
	logGL "newWorldRenderContext: genObjects"

	-- per layer buffer
	-- tile types
	mapM_ (\(layerBuffer, posBuffer, layerName) -> do
			print (world^.R.wTileIds layerName.wTileData)
			print (world^.R.wTilePos layerName.wPosData)
			uploadFromVec GL.ShaderStorageBuffer layerBuffer (world^.R.wTileIds layerName.wTileData)
			uploadFromVec GL.ShaderStorageBuffer posBuffer (world^.R.wTilePos layerName.wPosData)
		) $ zip3 layerBuffers posBuffers 
				(Map.keys $ world^.R.mapHashes.R.hashLayers)

	-- per image stuff
	mapM_ (\(i, texObject, (tsId, image)) -> do
		GL.activeTexture $= GL.TextureUnit i
		logGL "newWorldRenderContext: activeTexture"

		-- Make it the "currently bound 2D texture"
		GL.textureBinding GL.Texture2D $= Just texObject
		logGL "newWorldRenderContext: textureBinding"

		print (i, image)

		pngImage <- P.readPng (image^.R.iSource)
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
		) $ zip3 [0..] imageTextures (world^.R.wImages)


	let wrc = WorldRenderContext
		{ _wrcVao = vao
		, _wrcPosSSBs = foldr (uncurry Map.insert) Map.empty $ zip (Map.elems $ world^.R.mapHashes.R.hashLayers) posBuffers
		, _wrcLayerSSBs = foldr (uncurry Map.insert) Map.empty $ zip (Map.elems $ world^.R.mapHashes.R.hashLayers) layerBuffers

		, _wrcTextures = foldr (\((tsId, _), (i, tex)) -> Map.insert tsId (i, tex)) Map.empty $ zip (world^.R.wImages) $ zip [0..] imageTextures
		, _wrcTilesetSSB = tilesetBuffer
		, _wrcWorld = world
		}


	uploadFromVec GL.ShaderStorageBuffer tilesetBuffer (world^.wTilesetData wrc)

	print (world^.wTilesetData wrc)	

	return wrc


bindWorldRenderContext :: WorldRenderContext -> GL.Program -> IO ()
bindWorldRenderContext wrc _ = 
	GL.bindVertexArrayObject $= (Just $ wrc^.wrcVao)	

--renderNormalLayer :: GL.Program -> GL.BufferObject -> GL.BufferObject -> Layer -> IO ()
renderNormalLayer program layerSSB posSSB layerName world = do
	posIndex <- GL.getShaderStorageBlockIndex program "Pos"
	layerIndex <- GL.getShaderStorageBlockIndex program "LayerData"

	GL.bindBufferBase' GL.ShaderStorageBuffer posIndex posSSB
	logGL "renderNormalLayer: bindBufferBase' posIndex"
	GL.shaderStorageBlockBinding program posIndex posIndex	
	logGL "renderNormalLayer: shaderStorageBlockBinding posIndex"

	GL.bindBufferBase' GL.ShaderStorageBuffer layerIndex layerSSB
	logGL "renderNormalLayer: bindBufferBase' layerIndex"
	GL.shaderStorageBlockBinding program layerIndex layerIndex
	logGL "renderNormalLayer: shaderStorageBlockBinding layerIndex"

	print (posIndex, layerIndex)
	print (fromIntegral $ world^.R.wLayerNumObjects layerName)
	GL.drawArraysInstanced GL.Triangles 0 6 (fromIntegral (world^.R.wLayerNumObjects layerName))
	logGL "renderNormalLayer: drawArraysInstanced"

renderWorldRenderContext :: GL.Program -> WorldRenderContext -> IO ()
renderWorldRenderContext program wrc = do
	let world = wrc^.wrcWorld
	bindWorldRenderContext wrc program

	numTilesets <- GL.get $ GL.uniformLocation program "numTileSets"
	logGL "renderWorldRenderContext: uniformLoc numTilesets"
	GL.uniform numTilesets $= GL.Index1 (fromIntegral $ 
		Map.size (world^.R.mapTilesets) :: GL.GLint)
	logGL "renderWorldRenderContext: uniform numTilesets"

	tilesetIndex <- GL.getShaderStorageBlockIndex program "TileSets"
	logGL "renderWorldRenderContext: getShaderStorageBlockIndex"
	GL.bindBufferBase' GL.ShaderStorageBuffer tilesetIndex (wrc^.wrcTilesetSSB)
	logGL "renderWorldRenderContext: bindBufferBase' tilesetIndex"
	GL.shaderStorageBlockBinding program tilesetIndex tilesetIndex	
	logGL "renderWorldRenderContext: shaderStorageBlockBinding tilesetIndex"

	print (tilesetIndex, numTilesets)
	print $ Map.size (world^.R.mapTilesets)

	mapM_ (\i -> do
			sampler <- GL.get $ GL.uniformLocation program ("Texture" ++ show i)
			logGL "renderWorldRenderContext: uniformLoc sampler"
			GL.uniform sampler $= GL.TextureUnit (fromIntegral i)
			logGL "renderWorldRenderContext: uniform sampler"
		) [0..Map.size (wrc^.wrcTextures)-1]

	mapM_ (\(layerName, layerId) -> do
		let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
		let Just posBuf = wrc^.wrcPosSSBs.at layerId
		renderNormalLayer program layerBuf posBuf layerName world
		) (Map.toList $ world^.R.mapHashes.R.hashLayers)
