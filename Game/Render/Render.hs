module Game.Render.Render 
	(
	  setupShaders
	, uploadFromVec, updateFromVec
	) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Vector.Storable as V
import Data.Array.Storable
import Foreign.Storable.Tuple
import Foreign.Storable
import Foreign.Ptr
import qualified Graphics.UI.GLFW as GLFW
import System.FilePath ((</>))

import Data.Int

import qualified Data.ByteString as BS
import Graphics.Rendering.OpenGL
import Unsafe.Coerce (unsafeCoerce)

-- |Load a shader program from a file.
loadShader :: ShaderType -> FilePath -> IO Shader
loadShader st filePath = do
  shader <- createShader st
  BS.readFile filePath >>= (shaderSourceBS shader $=)
  compileShader shader
  ok <- get (compileStatus shader)
  infoLog <- get (shaderInfoLog shader)

  return shader

linkShaderProgramWith :: [Shader] -> IO Program
linkShaderProgramWith shaders = do
	p <- GL.createProgram
	mapM_ (GL.attachShader p) shaders
	GL.linkProgram p
	return p

uploadFromVec target buf vec = do
	GL.bindBuffer target $= Just buf
	V.unsafeWith vec $ \ptr ->
		GL.bufferData target $= (fromIntegral $ sizeOf(undefined::Float) * V.length vec, ptr, GL.DynamicDraw)

updateFromVec target buf vec = do
	GL.bindBuffer target $= Just buf
	V.unsafeWith vec $ \ptr ->
		GL.bufferSubData target WriteToBuffer 0 (fromIntegral $ sizeOf(undefined::Float) * V.length vec) ptr

setupShaders :: IO Program
setupShaders = do
	vs <- loadShader GL.VertexShader $ "data" </> "shader.vert"
	fs <- loadShader GL.FragmentShader $ "data" </> "shader.frag"
	linkShaderProgramWith [vs, fs]
	--(get $ shaderInfoLog vs) >>= print
	--(get $ shaderInfoLog fs) >>= print
	--(get $ programInfoLog prog) >>= print
