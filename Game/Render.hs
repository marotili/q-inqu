module Game.Render where

import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
--import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr

import Game.Render.Map
import Game.Render.Render
import Game.Render.Camera

data RenderContext = RenderContext
	{ rcMainProgram :: Program
	, rcWorldRenderContext :: WorldRenderContext
	, rcDebugBuffer :: GL.BufferObject
	--, rcCamera :: Camera
	}

newRenderContext gameMap = do
	program <- setupShaders

	wrc <- newWorldRenderContext gameMap
	bindWorldRenderContext wrc program

	[debugBuffer] <- GL.genObjectNames 1 :: IO [GL.BufferObject]

	uploadFromVec GL.ShaderStorageBuffer debugBuffer (V.fromList [0 | _ <- [0..4*6*81-1]] :: V.Vector Float)

	return RenderContext
		{ rcMainProgram = program
		, rcWorldRenderContext = wrc
		, rcDebugBuffer = debugBuffer
		}

clearWindow window = do
	GL.clearColor $= GL.Color4 1 1 1 1
	GL.clear [GL.ColorBuffer]
	(width, height) <- GLFW.getFramebufferSize window
	GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

render window rc cam = do
	-- update camera in every frame for now
	(width, height) <- GLFW.getFramebufferSize window
	clearWindow window

	GL.currentProgram $= Just (rcMainProgram rc)

	programSetViewProjection (rcMainProgram rc) cam
	--getShaderStorageBlockIndex (rcMainProgram rc) "Debug" >>= print
	--getShaderStorageBlockIndex (rcMainProgram rc) "Pos" >>= print
	--getShaderStorageBlockIndex (rcMainProgram rc) "Data" >>= print
	
	GL.bindBufferBase' GL.ShaderStorageBuffer 1 (rcDebugBuffer rc)
	GL.shaderStorageBlockBinding (rcMainProgram rc) 1 1


	renderWorldRenderContext (rcMainProgram rc) (rcWorldRenderContext rc)

	--errors <- GL.get GL.errors
	----print $ rcDebugBuffer rc
	--print errors
	--GL.bindBuffer GL.ShaderStorageBuffer $= Just (rcDebugBuffer rc)
	--GL.withMappedBuffer (GL.ShaderStorageBuffer) GL.ReadOnly printPtr failure

	where
		printPtr :: Ptr Float -> IO [()]
		printPtr ptr = sequence [
			peekElemOff ptr idx >>= print | idx <- [0..4*81*6-1]
			]

		failure GL.MappingFailed = print "Mapping failed" >> return []
		failure GL.UnmappingFailed = print "Unmapping failed" >> return []
