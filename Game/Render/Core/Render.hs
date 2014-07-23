{-# LANGUAGE TemplateHaskell #-}
module Game.Render.Core.Render 
	(
	) where

import Data.List
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.GL.Shaders.Program
import Graphics.Rendering.OpenGL.Raw
import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal
import System.FilePath ((</>))
import Game.Render.Core.Error
import Foreign.C

import Control.Lens
import qualified Data.ByteString as BS
import Graphics.Rendering.OpenGL

-- |Load a shader program from a file.
loadShader :: ShaderType -> FilePath -> IO Shader
loadShader st filePath = do
    shader <- createShader st

    BS.readFile filePath >>= (shaderSourceBS shader $=)
    compileShader shader

    compileStatus <- get (compileStatus shader)
    shaderLog <- get (shaderInfoLog shader)
    print (compileStatus, shaderLog)
    logGL "loadShader: loadShader"

    return shader

linkShaderProgramWith :: [Shader] -> IO Program
linkShaderProgramWith shaders = do
    p <- GL.createProgram
    logGL "linkShaderProgramWith: create program"

    mapM_ (GL.attachShader p) shaders
    logGL "linkShaderProgramWith: attach shaders"

    GL.linkProgram p
    logGL "linkShaderProgramWith: link program"

    return p

-- TODO: size of is hardcoded
uploadFromVec :: (Storable a) => Int -> GL.BufferTarget -> GL.BufferObject -> V.Vector a -> IO ()
uploadFromVec additionalBufferSize target buf vec = do
    GL.bindBuffer target $= Just buf
    logGL "uploadFromVec: bind buffer"
    V.unsafeWith vec $ \ptr ->
    	GL.bufferData target $= (fromIntegral $ sizeOf(undefined::Float) * (additionalBufferSize + V.length vec), ptr, GL.DynamicDraw)
    logGL "uploadFromVec: buffer data"

updateFromVec :: (Storable a) => GL.BufferTarget -> GL.BufferObject -> V.Vector a -> IO ()
updateFromVec target buf vec = do
    GL.bindBuffer target $= Just buf
    logGL "updateFromVec: bind buffer"
    V.unsafeWith vec $ \ptr ->
        GL.bufferSubData target WriteToBuffer (fromIntegral 0) (fromIntegral $ sizeOf(undefined::Float) * V.length vec) ptr
    logGL "updateFromVec: buffer sub data"

updateNewFromVec :: (Storable a) => GL.BufferTarget -> GL.BufferObject -> V.Vector a -> IO ()
updateNewFromVec target buf vec = do
    GL.bindBuffer target $= Just buf
    logGL "uploadFromVec: bind buffer"
    V.unsafeWith vec $ \ptr ->
        GL.bufferData target $= (fromIntegral $ sizeOf(undefined::Float) * (V.length vec), ptr, GL.DynamicDraw)
    logGL "uploadFromVec: buffer data"

setupShaders :: String -> String -> IO Program
setupShaders vertName fragName = do
    vs <- loadShader GL.VertexShader $ "data" </> "shaders" </> vertName
    logGL "setupShaders: loadShader"
    fs <- loadShader GL.FragmentShader $ "data" </> "shaders" </> fragName
    logGL "setupShaders: loadShader"
    prog <- linkShaderProgramWith [vs, fs]

    (get $ shaderInfoLog vs) >>= print
    (get $ shaderInfoLog fs) >>= print

    (get $ programInfoLog prog) >>= print

    return prog

data UniformInfo = UniformInfo
    { _uiName :: String
    , _uiType :: GLint
    , _uiSize :: GLint
    , _uiBlockIndex :: GLint
    , _uiOffset :: GLint
    } deriving (Show)

emptyInfo :: UniformInfo
emptyInfo = UniformInfo "" 0 0 0 0

makeLenses ''UniformInfo

uniformInfo :: Program -> IO [UniformInfo]
uniformInfo p = do
    numActiveUniforms <- alloca $ \buf -> do
         glGetProgramiv (programID p) gl_ACTIVE_UNIFORMS buf
         peek buf

    maxNameLength <- alloca $ \buf -> do
        glGetProgramiv (programID p) gl_ACTIVE_UNIFORM_MAX_LENGTH buf
        peek buf
    let maxNameLength' = fromIntegral maxNameLength
    let numActiveUniforms' = fromIntegral numActiveUniforms

    ptrs <- mapM (\_ -> mallocBytes maxNameLength') [0..numActiveUniforms'-1]
    let ptrList :: V.Vector (Ptr CChar)
        ptrList = V.fromList ptrs

    indicesPtr <- mallocBytes (4*numActiveUniforms') :: IO (Ptr GLuint)
    bufPtr <- mallocBytes (4*numActiveUniforms') :: IO (Ptr GLint)

    -- read uniform names
    mapM_ (\idx ->
            alloca $ \written -> alloca $ \size -> alloca $ \gltype ->
                glGetActiveUniform (programID p) (fromIntegral idx) maxNameLength written size gltype (ptrs!!idx)
        ) [0..numActiveUniforms'-1]

    names <- mapM peekCString ptrs

    let getters = [ gl_UNIFORM_TYPE
                    , gl_UNIFORM_SIZE
                    , gl_UNIFORM_NAME_LENGTH
                    , gl_UNIFORM_BLOCK_INDEX
                    , gl_UNIFORM_OFFSET
                    , gl_UNIFORM_ARRAY_STRIDE
                    , gl_UNIFORM_MATRIX_STRIDE
                    , gl_UNIFORM_IS_ROW_MAJOR
                    ]

    -- get uniform indices by name
    V.unsafeWith ptrList $ \namePtr ->
        glGetUniformIndices (programID p) numActiveUniforms namePtr indicesPtr

    uniformData <- mapM (\pname -> do
            glGetActiveUniformsiv (programID p) numActiveUniforms indicesPtr pname bufPtr
            mapM (peekElemOff bufPtr) [0..numActiveUniforms'-1]
        ) getters

    let uniformData' = fmap (\(gltype, size, bIdx, offset) -> 
                emptyInfo & uiType .~ gltype 
                          & uiSize .~ size
                          & uiBlockIndex .~ bIdx
                          & uiOffset .~ offset
            ) $ zip4 (head uniformData) (uniformData!!1) (uniformData!!3) (uniformData!!4)
    let uniformData'' = fmap (\(ud, name) -> ud & uiName .~ name) $ zip uniformData' names

    writeFile "uniform.log" ""
    mapM_ (\ud -> appendFile "uniform.log" $ show ud ++ "\n") uniformData''

    mapM_ free ptrs
    free bufPtr
    free indicesPtr

    return uniformData''

