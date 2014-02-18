{-# LANGUAGE TemplateHaskell #-}
module Game.Render.Light where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Lens
import qualified Data.Vector.Storable as V
import Game.Render.Render
import Game.Render.Camera
import Game.Render.Error
import Game.Collision
import Game.World.Visibility

import Game.World.Common

data LightContext = LightContext
	{ _lcVao :: GL.VertexArrayObject
	, _lcLightBuffer :: GL.BufferObject
	, _lcProgram :: GL.Program
	, _lcLights :: [Light]
	}

data Light = Light
	{ _lightPosition :: (Float, Float)
	, _lightIntensity :: Float
	, _lightFadeDistance :: Float
	}

data VisibilityContext = VisibilityContext
	{ _vcVao :: GL.VertexArrayObject
	, _vcProgram :: GL.Program
	, _vcPoints :: GL.BufferObject
	, _vcOctree :: GameOctree
	, _vcData :: [(Float, Float)]
	}

makeLenses ''VisibilityContext
makeLenses ''LightContext
makeLenses ''Light

newLight :: (Float, Float) -> Float -> Light
newLight pos intensity = Light
	{ _lightPosition = pos
	, _lightIntensity = intensity
	, _lightFadeDistance = intensity/2.0
	}

--newVisibilityContext octree playerPos :: IO VisibilityContext
newVisibilityContext octree playerPos = do
	[vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]
	[pointBuf] <- GL.genObjectNames 1 :: IO [GL.BufferObject]

	let pointData = playerPos : (getData octree playerPos)
	print $ pointData

	uploadFromVec GL.ShaderStorageBuffer pointBuf  $
		V.fromList $ concatMap (\(x, y) -> x:y:[]) pointData

	program <- setupShaders "shader.vis.vert" "shader.vis.frag"

	return VisibilityContext
		{ _vcVao = vao
		, _vcPoints = pointBuf
		, _vcProgram = program
		, _vcOctree = octree
		, _vcData = pointData
		}

updateVisibilityContext visCtx playerPos = do
	let pointData = playerPos : (getData (visCtx^.vcOctree) playerPos)
	updateFromVec GL.ShaderStorageBuffer (visCtx^.vcPoints) $
		V.fromList $ concatMap (\(x, y) -> x:y:[]) pointData
	return $ visCtx & vcData .~ pointData

renderVisibilityContext vc cam = do
	GL.currentProgram $= Just (vc^.vcProgram)
	logGL "renderLightContext: set currentProgram"

	programSetViewProjection (vc^.vcProgram) cam

	GL.bindVertexArrayObject $= (Just $ vc^.vcVao)
	logGL "renderLightContext: bind vao"
	lightIndex <- GL.getShaderStorageBlockIndex (vc^.vcProgram) "VisPoints"
	logGL "renderLightContext: light index"
	GL.bindBufferBase' GL.ShaderStorageBuffer lightIndex (vc^.vcPoints)

	GL.drawArraysInstanced GL.TriangleFan 0 (fromIntegral . length $ vc^.vcData) 1
	logGL "renderLightContext: draw instanced"

updateLightContext :: LightContext -> IO ()
updateLightContext lc =
	updateLights (lc^.lcLightBuffer) (lc^.lcLights)
	where
		updateLights :: GL.BufferObject -> [Light] -> IO () --V.Vector Float
		updateLights lightBuffer lights = do
			let dat = V.fromList $ concatMap toGl lights
			updateFromVec GL.UniformBuffer lightBuffer dat
			where
				toGl light = [light^.lightPosition._1, light^.lightPosition._2, light^.lightIntensity, 0]

renderLightContext :: LightContext -> Camera -> IO ()
renderLightContext lc cam = do
	GL.currentProgram $= Just (lc^.lcProgram)
	logGL "renderLightContext: set currentProgram"

	programSetViewProjection (lc^.lcProgram) cam

	numLights <- GL.get $ GL.uniformLocation (lc^.lcProgram) "numLights"
	logGL $ "renderLightContext: numLights, " ++ show numLights
	GL.uniform numLights $= GL.Index1 (1 :: GL.GLint)
	logGL "renderLightContext: set uniform numLights"

	GL.bindVertexArrayObject $= (Just $ lc^.lcVao)
	logGL "renderLightContext: bind vao"
	lightIndex <- GL.getUniformBlockIndex (lc^.lcProgram) "LightUBO"
	logGL "renderLightContext: light index"
	GL.bindBufferBase' GL.UniformBuffer lightIndex (lc^.lcLightBuffer)
	logGL "renderLightContext: bind buffer base"
	GL.uniformBlockBinding (lc^.lcProgram) lightIndex lightIndex	
	logGL "renderLightContext: uniform uniformBlockBinding"

	GL.drawArraysInstanced GL.Triangles 0 6 1
	logGL "renderLightContext: draw instanced"


newLightContext :: IO LightContext
newLightContext = do
	[vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]
	[lightsBuffer] <- GL.genObjectNames 1 :: IO [GL.BufferObject]

	let lights =
		[ newLight (-300, 500) 200
		, newLight (-400, 500) 100
		, newLight (0, 0) 0
		, newLight (0, 0) 0
		, newLight (0, 0) 0
		]

	uploadLights lightsBuffer lights

	program <- setupShaders "shader.light.vert" "shader.light.frag"
	_ <- uniformInfo program

	return LightContext
		{ _lcVao = vao
		, _lcLightBuffer = lightsBuffer
		, _lcLights = lights
		, _lcProgram = program
		}

	where
		uploadLights :: GL.BufferObject -> [Light] -> IO () --V.Vector Float
		uploadLights lightBuffer lights = do
			let dat = V.fromList $ concatMap toGl lights
			uploadFromVec GL.UniformBuffer lightBuffer dat
			where
				toGl light = [light^.lightPosition._1, light^.lightPosition._2, light^.lightIntensity, light^.lightFadeDistance]