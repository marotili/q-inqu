{-# LANGUAGE TemplateHaskell #-}
module Game.Render.Light where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Lens
import qualified Data.Vector.Storable as V
import Game.Render.Render
import Game.Render.Camera
import Game.Render.Error

data LightContext = LightContext
	{ _lcVao :: GL.VertexArrayObject
	, _lcLightBuffer :: GL.BufferObject
	, _lcProgram :: GL.Program
	, _lcLights :: [Light]
	}

data Light = Light
	{ _lightPosition :: (Float, Float)
	, _lightIntensity :: Float
	}

makeLenses ''LightContext
makeLenses ''Light

newLight :: (Float, Float) -> Float -> Light
newLight pos intensity = Light
	{ _lightPosition = pos
	, _lightIntensity = intensity
	}
uploadLights :: GL.BufferObject -> [Light] -> IO () --V.Vector Float
uploadLights lightBuffer lights = do
	let dat = V.fromList $ concatMap toGl lights
	uploadFromVec GL.UniformBuffer lightBuffer dat
	where
		toGl light = [light^.lightPosition._1, light^.lightPosition._2, light^.lightIntensity, 0]

renderLightContext :: LightContext -> Camera -> IO ()
renderLightContext lc cam = do
	GL.currentProgram $= Just (lc^.lcProgram)
	logGL "renderLightContext: set currentProgram"

	programSetViewProjection (lc^.lcProgram) cam

	numLights <- GL.get $ GL.uniformLocation (lc^.lcProgram) "numLights"
	logGL $ "renderLightContext: numLights, " ++ show numLights
	GL.uniform numLights $= GL.Index1 (2 :: GL.GLint)
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
		[ newLight (-300, 500) 100
		, newLight (-400, 500) 50
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