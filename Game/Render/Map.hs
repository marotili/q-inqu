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

import Foreign.C.Types
import Data.List
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import Graphics.Rendering.OpenGL (($=))
import Game.Render.Error
import qualified Codec.Picture as P
import Foreign.Ptr
import Control.Monad.State

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
	{ _wrcVaos :: Map.Map R.LayerId GL.VertexArrayObject
	, _wrcElements :: Map.Map R.LayerId GL.BufferObject

	, _wrcPosSSBs :: Map.Map R.LayerId GL.BufferObject
	, _wrcLayerSSBs :: Map.Map R.LayerId GL.BufferObject

	, _wrcTextures :: Map.Map R.TilesetId (Int, GL.TextureObject)
	, _wrcTilesetSSB :: GL.BufferObject
	, _wrcWorld :: R.World
	}

makeLenses ''WorldRenderContext
wPosData :: Getter [(Float, Float, Float)] (V.Vector Float)
wPosData = to (\poss -> V.fromList $ concatMap (\(x, y, a) -> concat . take 6 . repeat $ [x, y, a, 0]) poss)
wTileData :: Getter [Int] (V.Vector Int32)
wTileData = to (\ids -> V.fromList $ concatMap (\tId -> concat . take 6 . repeat $ [fromIntegral tId]) ids)

wTilesetData :: WorldRenderContext -> Getter R.World (V.Vector Int32)
wTilesetData wrc = to get
	where
		get world = V.fromList $ (map fromIntegral $ concat $
			sortBy tsInitialIdSort $ map(\(i, id, ts) -> tsData i ts id)
				$ zip3 [0..] ids tss) ++ padding
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
				padding = concat $ take (20 - length ids) $ repeat
					[ 0, 0, 0, 0, 0, 0, 0, 0
					, 0, 0, 0, 0, 0, 0, 0, 0]

wElementData :: R.World -> String -> V.Vector CUInt
wElementData world layerName = get
	where
		get = V.fromList $ map fromIntegral $
			concatMap (\(i, l) -> map ((+) (6*i)) l) $ zip [0..numObjects-1] (repeat [0, 1, 2, 3, 4, 5])
		numObjects = (world^.R.wLayerNumObjects layerName)

updateWorldRenderContext :: WorldRenderContext -> IO ()
updateWorldRenderContext wrc = do
	print (wrc^.wrcWorld.R.mapUpdateLayers)
	mapM_ (\layerId -> do
		let layerName = wrc^.wrcWorld.R.wLayerName layerId
		let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
		let Just posBuf = wrc^.wrcPosSSBs.at layerId 
		let Just elementBuf = wrc^.wrcElements.at layerId
		--print (wrc^.wrcWorld.R.wTileIds layerName.wTileData)
		--print (wrc^.wrcWorld.R.wTilePos layerName.wPosData)
		updateNewFromVec GL.UniformBuffer layerBuf 
			(wrc^.wrcWorld.R.wTileIds layerName.wTileData)
		updateNewFromVec GL.UniformBuffer posBuf 
			(wrc^.wrcWorld.R.wTilePos layerName.wPosData)
		uploadFromVec 0 GL.ElementArrayBuffer elementBuf 
			(wElementData (wrc^.wrcWorld) layerName)
		) (Set.toList $ wrc^.wrcWorld.R.mapUpdateLayers)

newWorldRenderContext :: R.World -> GL.Program -> IO WorldRenderContext
newWorldRenderContext world program = do
	posBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]
	layerBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]
	elementBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]

	[tilesetBuffer] <- GL.genObjectNames 1 :: IO [GL.BufferObject]
	vaos <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.VertexArrayObject]

	imageTextures <- GL.genObjectNames (Map.size (world^.R.mapTilesets)) :: IO [GL.TextureObject]
	logGL "newWorldRenderContext: genObjects"

	let wrc = execState (do
			wrcVaos .= Map.empty
			wrcElements .= Map.empty
			wrcPosSSBs .= Map.empty
			wrcLayerSSBs .= Map.empty
			wrcTextures .= Map.empty
			wrcTilesetSSB .= tilesetBuffer
			wrcWorld .= world

			mapM_ (\(layerId, layerBuf, posBuf, vao, element) -> do
					wrcLayerSSBs.at layerId .= (Just layerBuf)
					wrcPosSSBs.at layerId .= (Just posBuf)
					wrcVaos.at layerId .= Just vao
					wrcElements.at layerId .= Just element
				) $ zip5 (Map.keys $ world^.R.mapLayers) layerBuffers posBuffers vaos elementBuffers

			mapM_ (\(tilesetId, i, textureObj) -> do
				wrcTextures.at tilesetId .= Just (i, textureObj)
					) $ 
				zip3 (Map.keys $ world^.R.mapTilesets) [0..] imageTextures
		) $ WorldRenderContext {}

	-- per layer buffer
	-- tile types
	mapM_ (\layerId -> do
			let layerName = wrc^.wrcWorld.R.wLayerName layerId
			let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
			let Just posBuf = wrc^.wrcPosSSBs.at layerId
			let Just elementBuf = wrc^.wrcElements.at layerId
			--print (world^.R.wTileIds layerName.wTileData)
			--print (world^.R.wTilePos layerName.wPosData)
			print $ wElementData (wrc^.wrcWorld) layerName
			uploadFromVec 0 GL.ArrayBuffer layerBuf (world^.R.wTileIds layerName.wTileData)
			uploadFromVec 0 GL.ArrayBuffer posBuf (world^.R.wTilePos layerName.wPosData)
			uploadFromVec 0 GL.ElementArrayBuffer elementBuf (wElementData (wrc^.wrcWorld) layerName)
		) (Map.keys $ world^.R.mapLayers)

	-- per image stuff
	mapM_ (\tsId -> do
		let i = wrc^?!wrcTextures.at tsId._Just._1
		let texObject = wrc^?!wrcTextures.at tsId._Just._2
		let image = wrc^?!wrcWorld.R.mapTilesets.at tsId._Just.R.tsImage

		GL.activeTexture $= GL.TextureUnit (fromIntegral i)
		logGL "newWorldRenderContext: activeTexture"

		-- Make it the "currently bound 2D texture"
		GL.textureBinding GL.Texture2D $= Just texObject
		logGL "newWorldRenderContext: textureBinding"

		--print (i, image)

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
		) $ (Map.keys $ world^.R.mapTilesets)



	uploadFromVec 0 GL.UniformBuffer tilesetBuffer (world^.wTilesetData wrc)

	--print ("Tsdata", world^.wTilesetData wrc)

	mapM_ (\layerId -> do
			let layerName = wrc^.wrcWorld.R.wLayerName layerId
			let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
			let Just posBuf = wrc^.wrcPosSSBs.at layerId
			let Just vao = wrc^.wrcVaos.at layerId

			GL.bindVertexArrayObject $= (Just vao)	
			GL.AttribLocation tileIdLoc <- GL.get $ GL.attribLocation program "tileId"
			GL.AttribLocation posLoc <- GL.get $ GL.attribLocation program "pos"
			GL.AttribLocation rotationLoc <- GL.get $ GL.attribLocation program "rotation"

			--print ("Locations", vao, layerBuf, posBuf, tileIdLoc, posLoc, rotationLoc)

			GL.bindBuffer GL.ArrayBuffer $= Just layerBuf
			GLRaw.glVertexAttribIPointer tileIdLoc 1 GLRaw.gl_INT 0 (nullPtr)
			--GLRaw.glVertexAttribDivisor tileIdLoc 3
			GLRaw.glEnableVertexAttribArray tileIdLoc

			GL.bindBuffer GL.ArrayBuffer $= Just posBuf
			GLRaw.glVertexAttribPointer posLoc 2 GLRaw.gl_FLOAT 0 16 (nullPtr)
			--GLRaw.glVertexAttribDivisor posLoc 3
			GLRaw.glEnableVertexAttribArray posLoc

			GLRaw.glVertexAttribPointer rotationLoc 1 GLRaw.gl_FLOAT 0 16 (plusPtr nullPtr 8)
			--GLRaw.glVertexAttribDivisor rotationLoc 3
			GLRaw.glEnableVertexAttribArray rotationLoc
			return ()
		) (Map.keys $ world^.R.mapLayers)

	return wrc


bindWorldRenderContext :: WorldRenderContext -> GL.Program -> IO ()
bindWorldRenderContext wrc program = return ()

--renderNormalLayer :: GL.Program -> GL.BufferObject -> GL.BufferObject -> Layer -> IO ()
renderNormalLayer program wrc layerName layerId world = do
	let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
	let Just posBuf = wrc^.wrcPosSSBs.at layerId
	let Just vao = wrc^.wrcVaos.at layerId
	let Just element = wrc^.wrcElements.at layerId

	GL.bindVertexArrayObject $= (Just vao)

	print ((world^.R.wLayerNumObjects layerName), layerName)

	GL.bindBuffer GL.ElementArrayBuffer $= Just element

	GLRaw.glDrawElements GLRaw.gl_TRIANGLES (6*(fromIntegral (world^.R.wLayerNumObjects layerName))) GLRaw.gl_UNSIGNED_INT nullPtr
	logGL "renderNormalLayer: drawArraysInstanced"

renderWorldRenderContext :: GL.Program -> WorldRenderContext -> IO ()
renderWorldRenderContext program wrc = do
	--print "get world"
	let world = wrc^.wrcWorld
	--print "bind wrc"
	bindWorldRenderContext wrc program

	--print "bind num tilesets"
	numTilesets <- GL.get $ GL.uniformLocation program "numTileSets"
	logGL "renderWorldRenderContext: uniformLoc numTilesets"
	GL.uniform numTilesets $= GL.Index1 (fromIntegral $ 
		Map.size (world^.R.mapTilesets) :: GL.GLint)
	logGL "renderWorldRenderContext: uniform numTilesets"


	--print "bind tilesets"
	tilesetIndex <- GL.getUniformBlockIndex program "TileSets"
	logGL "renderWorldRenderContext: getUniformBlockIndex"
	GL.bindBufferBase' GL.UniformBuffer tilesetIndex (wrc^.wrcTilesetSSB)
	logGL "renderWorldRenderContext: bindBufferBase' tilesetIndex"
	GL.uniformBlockBinding program tilesetIndex tilesetIndex	
	logGL "renderWorldRenderContext: uniformBlockBinding tilesetIndex"

	--print (tilesetIndex, numTilesets)
	--print $ Map.size (world^.R.mapTilesets)

	mapM_ (\i -> do
			sampler <- GL.get $ GL.uniformLocation program ("Texture" ++ show i)
			logGL "renderWorldRenderContext: uniformLoc sampler"
			GL.uniform sampler $= GL.TextureUnit (fromIntegral i)
			logGL "renderWorldRenderContext: uniform sampler"
		) [0..Map.size (wrc^.wrcTextures)-1]

	mapM_ (\(layerName, layerId) -> do
		renderNormalLayer program wrc layerName layerId world
		) (Map.toList $ world^.R.mapHashes.R.hashLayers)
