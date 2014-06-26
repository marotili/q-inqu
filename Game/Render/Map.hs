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

import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Debug.Trace
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
--import qualified Data.Vector.Storable.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Game.Render.Render

import Data.Binary.IEEE754

import qualified Data.Tiled as T
import Data.Tiled
import Game.World.Import.Tiled

import Control.Lens
import qualified Game.Render.World as R


data WorldRenderContext = WorldRenderContext
	{ _wrcVaos :: Map.Map R.LayerId GL.VertexArrayObject
	, _wrcElements :: Map.Map R.LayerId GL.BufferObject

	, _wrcMeshSSBs :: Map.Map R.LayerId GL.BufferObject

	, _wrcPosSSBs :: Map.Map R.LayerId GL.BufferObject
	, _wrcLayerSSBs :: Map.Map R.LayerId GL.BufferObject

	, _wrcTextures :: Map.Map R.TilesetId (Int, GL.TextureObject)
	, _wrcTilesetSSB :: GL.BufferObject
	, _wrcWorld :: R.World
	}

makeLenses ''WorldRenderContext
wPosData :: Getter [(Float, Float, Float, Float, Float)] (V.Vector Word32)
wPosData = to (\poss -> V.fromList $ concatMap (\(x, y, ox, oy, a) -> concat . take 6 . repeat . map floatToWord $ [x, y, ox, oy, a, 0, 0, 0]) poss)
wTileData :: Getter [Int] (V.Vector Int32)
wTileData = to (\ids -> V.fromList $ concatMap (\tId -> concat . take 6 . repeat $ [fromIntegral tId]) ids)

-- :: (Binary a) => a -> [Word8]
-- a = traceShow (length $ BL.unpack . Data.Binary.encode $ a) $ BL.unpack . Data.Binary.encode $ a


wMeshData :: WorldRenderContext -> Getter [(Float, Float, Float, Float, Float, Float, Int)] (V.Vector Word32)
wMeshData wrc = to (\meshs -> V.fromList $ concatMap
	(\(px, py, sx', sy', imgW, imgH, tsId) ->
		let
			sx = floatToWord sx'
			sy = floatToWord sy'
			tx0 = (floatToWord $ ((px) / imgW)) ::  Word32
			tx1 = (floatToWord $ (px + sx') / imgW) ::  Word32
			ty0 = (floatToWord $ ((py) / imgH)) ::  Word32
			ty1 = (floatToWord $ (py + sy') / imgH) ::  Word32
			imgId = (fromIntegral . fromJust $ wrc^?wrcTextures.at tsId._Just._1) :: Word32
			zero = 0 :: Word32
		in let x =
			[ zero, zero, tx0, ty1, imgId, zero, zero, zero
			, sx, zero, tx1, ty1, imgId, zero, zero, zero
			, sx, sy, tx1, ty0, imgId, zero, zero, zero
			, sx, sy, tx1, ty0, imgId, zero, zero, zero
			, zero, sy, tx0, ty0, imgId, zero, zero, zero
			, zero, zero, tx0, ty1, imgId, zero, zero, zero
			] in x
		) meshs
	)

wTilesetData :: WorldRenderContext -> Getter R.World (V.Vector Int32)
wTilesetData wrc = to get
	where
		get world = V.fromList $ (map fromIntegral $ concat $
			sortBy tsInitialIdSort $ map(\(i, id, mts) ->
				case mts of
					Just ts -> tsData i ts id
					Nothing -> error "tileset not found"
					)
				$ zip3 [0..] ids tss) ++ padding
			where
				tsInitialIdSort (tsId:xs) (tsId2:ys)
					| tsId < tsId2 = LT
					| otherwise = GT
				ids = Map.keys (world^.R.mapTilesets)
				tss = map (\tsId -> world^.R.mapTilesets.at tsId) ids
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
	--print (wrc^.wrcWorld.R.mapUpdateLayers)
	mapM_ (\layerId -> do
		let layerName = wrc^.wrcWorld.R.wLayerName layerId
		let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
		let Just posBuf = wrc^.wrcPosSSBs.at layerId
		let Just elementBuf = wrc^.wrcElements.at layerId
		let Just meshBuf = wrc^.wrcMeshSSBs.at layerId

		updateFromVec GL.ArrayBuffer layerBuf
			(wrc^.wrcWorld.R.wTileIds layerName.wTileData)
		updateFromVec GL.ArrayBuffer posBuf
			(wrc^.wrcWorld.R.wTilePos layerName.wPosData)
		updateFromVec GL.ArrayBuffer meshBuf
			(wrc^.wrcWorld.R.wTileMesh layerName.wMeshData wrc)
		updateFromVec GL.ElementArrayBuffer elementBuf 
			(wElementData (wrc^.wrcWorld) layerName)
		) (Set.toList $ wrc^.wrcWorld.R.mapUpdateLayers)

newWorldRenderContext :: R.World -> GL.Program -> IO WorldRenderContext
newWorldRenderContext world program = do
	posBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]
	layerBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]
	elementBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]
	meshBuffers <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.BufferObject]
	logGL "newWorldRenderContext: genBuffers"

	[tilesetBuffer] <- GL.genObjectNames 1 :: IO [GL.BufferObject]
	vaos <- GL.genObjectNames (Map.size (world^.R.mapLayers)) :: IO [GL.VertexArrayObject]
	logGL "newWorldRenderContext: genVaos"

	imageTextures <- GL.genObjectNames (
		Map.size (world^.R.mapTilesets) +
		Map.size (world^.R.mapComplexTilesets)
		) :: IO [GL.TextureObject]
	logGL "newWorldRenderContext: Textures"

	let wrc = execState (do
			wrcVaos .= Map.empty
			wrcElements .= Map.empty
			wrcMeshSSBs .= Map.empty
			wrcPosSSBs .= Map.empty
			wrcLayerSSBs .= Map.empty
			wrcTextures .= Map.empty
			wrcTilesetSSB .= tilesetBuffer
			wrcWorld .= world

			mapM_ (\(layerId, layerBuf, posBuf, meshBuf, vao, element) -> do
					wrcLayerSSBs.at layerId .= (Just layerBuf)
					wrcPosSSBs.at layerId .= (Just posBuf)
					wrcMeshSSBs.at layerId .= (Just meshBuf)
					wrcVaos.at layerId .= Just vao
					wrcElements.at layerId .= Just element
				) $ zip6 (Map.keys $ world^.R.mapLayers) layerBuffers posBuffers meshBuffers vaos elementBuffers

			mapM_ (\(tilesetId, i, textureObj) -> do
				wrcTextures.at tilesetId .= Just (i, textureObj)
					) $
				zip3 (
					(Map.keys $ world^.R.mapTilesets) ++
					(Map.keys $ world^.R.mapComplexTilesets)
					) [0..] imageTextures
		) $ WorldRenderContext {}

	-- per layer buffer
	-- tile types
	mapM_ (\layerId -> do
			let layerName = wrc^.wrcWorld.R.wLayerName layerId
			let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
			let Just posBuf = wrc^.wrcPosSSBs.at layerId
			let Just meshBuf = wrc^.wrcMeshSSBs. at layerId
			let Just elementBuf = wrc^.wrcElements.at layerId

			uploadFromVec 1000 GL.ArrayBuffer layerBuf (world^.R.wTileIds layerName.wTileData)
			uploadFromVec 1000 GL.ArrayBuffer posBuf (world^.R.wTilePos layerName.wPosData)
			uploadFromVec 1000 GL.ArrayBuffer meshBuf (world^.R.wTileMesh layerName.wMeshData wrc)
			uploadFromVec 1000 GL.ElementArrayBuffer elementBuf (wElementData (wrc^.wrcWorld) layerName)

		) (Map.keys $ world^.R.mapLayers)

	-- per image stuff
	mapM_ (\tsId -> do
		let Just i = wrc^?wrcTextures.at tsId._Just._1
		let Just texObject = wrc^?wrcTextures.at tsId._Just._2

		let Just image = if (Map.member tsId $ wrc^.wrcWorld.R.mapTilesets) then
			wrc^?wrcWorld.R.mapTilesets.at tsId._Just.R.tsImage
			else
				wrc^?wrcWorld.R.mapComplexTilesets.at tsId._Just.R.ctsImage

		GL.activeTexture $= GL.TextureUnit (fromIntegral i)
		logGL "newWorldRenderContext: activeTexture"

		-- Make it the "currently bound 2D texture"
		GL.textureBinding GL.Texture2D $= Just texObject
		logGL "newWorldRenderContext: textureBinding"

		pngImage <- P.readPng (image^.R.iSource)
		case pngImage of
			Left err -> error (show ("failed to read", err, image^.R.iSource))
			Right s -> case s of
				(P.ImageRGBA8 (P.Image imgWidth imgHeight dat)) ->
					V.unsafeWith dat $ \ptr -> do
						GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8
							(GL.TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0
							(GL.PixelData GL.RGBA GL.UnsignedByte ptr)
						logGL "newWorldRenderContext: texImage2D"
						GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
						logGL "newWorldRenderContext: textureFilter"
				(P.ImageRGB8 _) -> error ("ImageRGB8 not supported")
				(P.ImageRGBA16 _) -> error ("ImageRGBA16 not supported")
				(P.ImageY8 _) -> error ("ImageY8 not supported")
				(P.ImageYA8 _) -> error ("ImageYA8 not supported")
				(P.ImageYCbCr8 _) -> error ("ImageYCbCr8 not supported")
				_ -> error (show ("Only RGBA8 supported", image))
		) $ (Map.keys $ world^.R.mapTilesets) ++ (Map.keys $ world^.R.mapComplexTilesets)

	uploadFromVec 0 GL.UniformBuffer tilesetBuffer (world^.wTilesetData wrc)

	--print ("Tsdata", world^.wTilesetData wrc)

	mapM_ (\layerId -> do
			let layerName = wrc^.wrcWorld.R.wLayerName layerId
			let isComplexLayer = wrc^.wrcWorld.R.wIsComplexLayer layerId
			let Just layerBuf = wrc^.wrcLayerSSBs.at layerId
			let Just meshBuf = wrc^.wrcMeshSSBs.at layerId
			let Just posBuf = wrc^.wrcPosSSBs.at layerId
			let Just vao = wrc^.wrcVaos.at layerId

			GL.bindVertexArrayObject $= (Just vao)
			GL.AttribLocation tileIdLoc <- GL.get $ GL.attribLocation program "tileId"
			GL.AttribLocation posLoc <- GL.get $ GL.attribLocation program "pos"
			GL.AttribLocation originLoc <- GL.get $ GL.attribLocation program "origin"
			GL.AttribLocation rotationLoc <- GL.get $ GL.attribLocation program "rotation"

			GL.AttribLocation mesh <- GL.get $ GL.attribLocation program "mesh"
			GL.AttribLocation texCoordsIn <- GL.get $ GL.attribLocation program "myTexCoords"
			GL.AttribLocation imageIn <- GL.get $ GL.attribLocation program "thisIsImage"

			--print ("Locations", vao, layerBuf, posBuf, tileIdLoc, posLoc, rotationLoc)

			GL.bindBuffer GL.ArrayBuffer $= Just layerBuf
			GLRaw.glVertexAttribIPointer tileIdLoc 1 GLRaw.gl_INT 0 (nullPtr)
			--GLRaw.glVertexAttribDivisor tileIdLoc 3
			when (not isComplexLayer) $
				GLRaw.glEnableVertexAttribArray tileIdLoc

			GL.bindBuffer GL.ArrayBuffer $= Just posBuf
			GLRaw.glVertexAttribPointer posLoc 2 GLRaw.gl_FLOAT 0 32 (nullPtr)
			--GLRaw.glVertexAttribDivisor posLoc 3
			GLRaw.glEnableVertexAttribArray posLoc

			GLRaw.glVertexAttribPointer originLoc 2 GLRaw.gl_FLOAT 0 32 (plusPtr nullPtr 8)
			--GLRaw.glVertexAttribDivisor posLoc 3
			GLRaw.glEnableVertexAttribArray originLoc

			GLRaw.glVertexAttribPointer rotationLoc 1 GLRaw.gl_FLOAT 0 32 (plusPtr nullPtr 16)
			--GLRaw.glVertexAttribDivisor rotationLoc 3

			GLRaw.glEnableVertexAttribArray rotationLoc

			--print (posLoc, originLoc, rotationLoc, tileIdLoc)

			alloca $ \ptr -> do
				GLRaw.glGetIntegerv GLRaw.gl_MAX_VERTEX_ATTRIBS ptr
				val <- peek ptr
				print val


			GL.bindBuffer GL.ArrayBuffer $= Just meshBuf
			GLRaw.glVertexAttribPointer mesh 2 GLRaw.gl_FLOAT 0 32 (nullPtr)
			--GLRaw.glVertexAttribDivisor posLoc 3

			GLRaw.glVertexAttribPointer texCoordsIn 2 GLRaw.gl_FLOAT 0 32 (plusPtr nullPtr 8)
			--GLRaw.glVertexAttribDivisor posLoc 3

			GLRaw.glVertexAttribIPointer imageIn 1 GLRaw.gl_INT 32 (plusPtr nullPtr 16)
			--GLRaw.glVertexAttribDivisor rotationLoc 3

			when (isComplexLayer) $ do
				GLRaw.glEnableVertexAttribArray mesh
				GLRaw.glEnableVertexAttribArray texCoordsIn
				GLRaw.glEnableVertexAttribArray imageIn

			return ()
		) (Map.keys $ world^.R.mapLayers)

	return wrc


bindWorldRenderContext :: WorldRenderContext -> GL.Program -> IO ()
bindWorldRenderContext wrc program = return ()

--renderNormalLayer :: GL.Program -> GL.BufferObject -> GL.BufferObject -> Layer -> IO ()
renderNormalLayer program wrc layerName layerId world = do
	let Just element = wrc^.wrcElements.at layerId
	let Just vao = wrc^.wrcVaos.at layerId

	GL.bindVertexArrayObject $= (Just vao)

	--print ((world^.R.wLayerNumObjects layerName), layerName)

	GL.bindBuffer GL.ElementArrayBuffer $= Just element

	GLRaw.glDrawElements GLRaw.gl_TRIANGLES (6*(fromIntegral (world^.R.wLayerNumObjects layerName))) GLRaw.gl_UNSIGNED_INT nullPtr
	logGL "renderNormalLayer: drawArraysInstanced"

--renderWorldRenderContext :: GL.Program -> WorldRenderContext -> IO ()
renderWorldRenderContext program wrc playerId = do
	--print "get world"
	let world = wrc^.wrcWorld
	--print "bind wrc"
	bindWorldRenderContext wrc program

	--print "bind num tilesets"
	numTilesets <- GL.get $ GL.uniformLocation program "numTileSets"
	logGL "renderWorldRenderContext: uniformLoc numTilesets"
	GL.uniform numTilesets $= (GL.Index1 (fromIntegral $ Map.size (world^.R.mapTilesets)) :: GL.Index1 GL.GLint)
	logGL "renderWorldRenderContext: uniform numTilesets"

	--print "bind tilesets"
	tilesetIndex <- GL.getUniformBlockIndex program "TileSets"
	logGL "renderWorldRenderContext: getUniformBlockIndex"
	GL.bindBufferBase' GL.UniformBuffer tilesetIndex (wrc^.wrcTilesetSSB)
	logGL "renderWorldRenderContext: bindBufferBase' tilesetIndex"
	GL.uniformBlockBinding program tilesetIndex tilesetIndex
	logGL "renderWorldRenderContext: uniformBlockBinding tilesetIndex"

	isComplex <- GL.get $ GL.uniformLocation program "isComplex"

	ghostMode <- GL.get $ GL.uniformLocation program "ghostMode"
	GL.uniform ghostMode $= GL.Index1 (fromIntegral playerId :: GL.GLint)

	--print $ Map.size (world^.R.mapTilesets)

	mapM_ (\i -> do
			sampler <- GL.get $ GL.uniformLocation program ("Texture" ++ show i)
			logGL "renderWorldRenderContext: uniformLoc sampler"
			GL.uniform sampler $= GL.TextureUnit (fromIntegral i)
			logGL "renderWorldRenderContext: uniform sampler"
		) [0..Map.size (wrc^.wrcTextures)-1]

	mapM_ (\(layerName, layerId) -> do
		if (wrc^.wrcWorld.R.wIsComplexLayer layerId) then
			GL.uniform isComplex $= GL.Index1 (1 :: GL.GLint)
			else GL.uniform isComplex $= GL.Index1 (0 :: GL.GLint)

		renderNormalLayer program wrc layerName layerId world
		) (Map.toList $ world^.R.mapHashes.R.hashLayers)
