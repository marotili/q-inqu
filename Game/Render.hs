module Game.Render where

import Graphics.Rendering.OpenGL
--import Graphics.Rendering.OpenGL.Texturing.Filter
import qualified Graphics.Rendering.OpenGL as GL
--import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr

import Game.Render.Map
import Game.Render.Render
import Game.Render.Camera
import Codec.Picture.Png
import Codec.Picture

import System.Exit

data RenderContext = RenderContext
	{ rcMainProgram :: Program
	, rcWorldRenderContext :: WorldRenderContext
	, rcDebugBuffer :: GL.BufferObject
	--, rcCamera :: Camera
	}

newRenderContext gameMap renderMap = do
	program <- setupShaders

	wrc <- newWorldRenderContext gameMap renderMap
	bindWorldRenderContext wrc program

	[debugBuffer] <- GL.genObjectNames 1 :: IO [GL.BufferObject]

	uploadFromVec GL.ShaderStorageBuffer debugBuffer (V.fromList [0 | _ <- [0..4*6*81-1]] :: V.Vector Float)


	-- Generate 1 texture object
	[texObject] <- genObjectNames 1

	activeTexture $= TextureUnit 0

	-- Make it the "currently bound 2D texture"
	textureBinding Texture2D $= Just texObject

	image <- readPng "data/sewer_tileset.png"
	--let imgWidth = 192
	--let imgHeight = 217
	case image of
		(Left s) -> do
			print s
			exitWith (ExitFailure 1)
		(Right s) -> case s of
			(ImageRGBA8 (Image imgWidth imgHeight dat)) -> do
				print $ (imgWidth, imgHeight)
				V.unsafeWith dat $ \ptr -> do
					texImage2D Texture2D NoProxy 0 RGBA8 
						(TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0 
						(PixelData RGBA UnsignedByte ptr)
					textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
			--(ImageRGB8 (Image imgWidth imgHeight dat)) ->
			--	V.unsafeWith dat $ \ptr ->
			--		texImage2D Texture2D NoProxy 0 RGB8
			--			(TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0 
			--			(PixelData RGB UnsignedByte ptr)

	--err <- get GL.errors
	--print err

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
