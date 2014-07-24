{-# LANGUAGE TemplateHaskell #-}
module Game.Render.Core.Manager 
(
)
where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import Graphics.Rendering.OpenGL (($=))
import qualified Data.Set as Set
import qualified Codec.Picture as P
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Vector.Storable as V
import Control.Monad.Morph
import Data.Binary.IEEE754
import Data.Word

import Foreign.C.Types
import Foreign.Ptr

import Game.Render.Core.Text
import Game.Render.Core.Error
import Game.Render.Core.Render
import Game.Render.Core.Camera

data RenderManager = RenderManager
    { _rmUsedTextures :: Set.Set GL.TextureUnit
    }
newRenderManager = RenderManager Set.empty


makeLenses ''RenderManager

--generalize :: Monad m => Identity a -> m a
--generalize = return . runIdentity

type Position = (Float, Float)
type Color = (Float, Float, Float, Float)

data Triangle = Triangle 
    { _triangleV0 :: (Position, Color)
    , _triangleV1 :: (Position, Color)
    , _triangleV2 :: (Position, Color)
    }
makeLenses ''Triangle

-- | creates a list for the vertex array
triangleToList :: Triangle -> [Word32]
triangleToList triangle = map floatToWord
    -- | zero for padding
    [ x0, y0, r0, g0, b0, a0, 0, 0
    , x1, y1, r1, g1, b1, a1, 0, 0
    , x2, y2, r2, g2, b2, a2, 0, 0
    ]
    where
        (x0, y0) = triangle^.triangleV0._1
        (x1, y1) = triangle^.triangleV1._1
        (x2, y2) = triangle^.triangleV2._1
        (r0, g0, b0, a0) = triangle^.triangleV0._2
        (r1, g1, b1, a1) = triangle^.triangleV1._2
        (r2, g2, b2, a2) = triangle^.triangleV2._2

data TriangleRenderer = TriangleRenderer
    { _triangleData :: [Triangle]
    , _triangleBuffer :: GL.BufferObject
    , _triangleBufferSize :: Int
    , _triangleElements :: GL.BufferObject
    , _triangleVAO :: GL.VertexArrayObject
    , _triangleShader :: GL.Program
    }
makeLenses ''TriangleRenderer

-- | return the total number of vertices inside the renderer
-- | note: equals the length of elements in triangleElements
trNumVertices :: Getter TriangleRenderer Int
trNumVertices = to (\tr -> 3*length (tr^.triangleData))

-- | Create a static renderer for triangles
newTriangleRenderer :: [Triangle] -> IO TriangleRenderer
newTriangleRenderer triangles = do
    [buffer, elementBuffer] <- GL.genObjectNames 2 :: IO [GL.BufferObject]
    [vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]
    program <- setupShaders "untextured.vert" "untextured.frag"
    GL.currentProgram $= (Just program)
    GL.bindFragDataLocation program "color_final" $= 0
    logGL "triangle renderer setup shaders"
    uniformInfo program

    let vertexData = V.fromList $ concatMap triangleToList triangles
    let elementData = V.fromList $ map fromIntegral $ concatMap (\(i, l) -> map ((3*i) +) l) $ zip [0..length triangles - 1] (repeat [0, 1, 2]) :: V.Vector CUInt
    uploadFromVec (V.length vertexData) GL.ArrayBuffer buffer vertexData
    uploadFromVec (3*length triangles) GL.ElementArrayBuffer elementBuffer elementData
    logGL "upload data"

    GL.bindVertexArrayObject $= Just vao
    logGL "bind vao"
    GL.AttribLocation posLoc <- GL.get $ GL.attribLocation program "pos"
    logGL "get pos loc"
    GL.AttribLocation colorLoc <- GL.get $ GL.attribLocation program "myColor"
    logGL "get color loc"
    print colorLoc
    print posLoc

    GL.bindBuffer GL.ArrayBuffer $= Just buffer
    logGL "bind array buffer"
    GLRaw.glVertexAttribPointer posLoc 2 GLRaw.gl_FLOAT 0 32 nullPtr
    logGL "set pos loc"
    GLRaw.glEnableVertexAttribArray posLoc
    logGL "enable pos attrib"

    GLRaw.glVertexAttribPointer colorLoc 4 GLRaw.gl_FLOAT 0 32 (plusPtr nullPtr 8)
    logGL "set color loc"
    GLRaw.glEnableVertexAttribArray colorLoc
    logGL "enable color attrib"

    print vertexData
    print elementData

    return $ TriangleRenderer
        { _triangleData = triangles
        , _triangleBuffer = buffer
        , _triangleBufferSize = V.length vertexData
        , _triangleElements = elementBuffer
        , _triangleVAO = vao
        , _triangleShader = program
        }

-- | render all triangles stored in triangle renderer
-- | note: set camera first
renderTriangles :: TriangleRenderer -> Camera -> IO ()
renderTriangles renderer cam = do
    GL.currentProgram $= Just (renderer^.triangleShader)
    programSetViewProjection (renderer^.triangleShader) cam

    GL.bindVertexArrayObject $= Just (renderer^.triangleVAO)
    GL.bindBuffer GL.ElementArrayBuffer $= Just (renderer^.triangleElements)
    --GLRaw.glDrawElements GLRaw.gl_TRIANGLES (fromIntegral $ renderer^.trNumVertices) GLRaw.gl_UNSIGNED_INT nullPtr
    GLRaw.glDrawElements GLRaw.gl_TRIANGLES 3 GLRaw.gl_UNSIGNED_INT nullPtr


data FontRenderer = FontRenderer
    { _frTextAtlas :: TextAtlas
    , _frAtlasTexture :: GL.TextureObject
    , _frAtlasTextureUnit :: Maybe GL.TextureUnit
    , _frProgram :: GL.Program
    , _frVAO :: GL.VertexArrayObject
    , _frTopoBuffer :: GL.BufferObject
    , _frColorBuffer :: GL.BufferObject
    , _frElementBuffer :: GL.BufferObject
    , _frAtlasUniform :: GL.BufferObject
    }
makeLenses ''FontRenderer

-- | get a new texture unit
mkTextureUnit :: State RenderManager GL.TextureUnit
mkTextureUnit = do
    usedTextures <- use rmUsedTextures
    let unit = head $ filter (\unit -> not $ Set.member unit usedTextures) . map GL.TextureUnit $ [0..]
    rmUsedTextures %= Set.insert unit
    return unit

-- | initialize a new font renderer using a loaded text atlas
newFontRenderer :: TextAtlas -> StateT RenderManager IO FontRenderer
newFontRenderer textAtlas = do
    textureUnit <- hoist generalize mkTextureUnit
    lift $ do
        [topoBuffer, colorBuffer, elementBuffer, atlasBuffer] <- GL.genObjectNames 4 :: IO [GL.BufferObject]
        [vao] <- GL.genObjectNames 1 :: IO [GL.VertexArrayObject]
        program <- setupShaders "text.vert" "text.frag"
        GL.currentProgram $= Just program

        let vertexData = (V.fromList $ concat $ replicate 6 [floatToWord 2, floatToWord 0, 33, 0] ++ replicate 6 [floatToWord 77, floatToWord 0, 0, 0])
        let elementData = V.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11] :: V.Vector Word32
        let atlasData = atlasToStorable textAtlas
        uploadFromVec (V.length vertexData) GL.ArrayBuffer topoBuffer vertexData
        uploadFromVec (V.length elementData) GL.ElementArrayBuffer elementBuffer elementData
        uploadFromVec (V.length atlasData) GL.UniformBuffer atlasBuffer atlasData
        logGL "upload data"


        GL.bindVertexArrayObject $= (Just vao)
        GL.AttribLocation posLoc <- GL.get $ GL.attribLocation program "pos"
        GL.AttribLocation myColorLoc <- GL.get $ GL.attribLocation program "myColor"
        GL.AttribLocation charIdLoc <- GL.get $ GL.attribLocation program "charId"

        GL.bindBuffer GL.ArrayBuffer $= Just topoBuffer
        GLRaw.glVertexAttribPointer posLoc 2 GLRaw.gl_FLOAT 0 16 nullPtr
        GLRaw.glEnableVertexAttribArray posLoc

        GLRaw.glVertexAttribIPointer charIdLoc 1 GLRaw.gl_INT 16 (plusPtr nullPtr 8)
        GLRaw.glEnableVertexAttribArray charIdLoc

        [imageTexture] <- GL.genObjectNames 1 :: IO [GL.TextureObject]

        GL.activeTexture $= textureUnit
        logGL "newWorldRenderContext: activeTexture"

        -- Make it the "currently bound 2D texture"
        GL.textureBinding GL.Texture2D $= Just imageTexture
        logGL "newWorldRenderContext: textureBinding"

        let image = textAtlas^.atlasImage

        case image of
            (P.ImageRGBA8 (P.Image imgWidth imgHeight dat)) ->
                V.unsafeWith dat $ \ptr -> do
                    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8
                        (GL.TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0
                        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
                    logGL "newWorldRenderContext: texImage2D"
                    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
                    logGL "newWorldRenderContext: textureFilter"
            (P.ImageRGB8 _) -> error "ImageRGB8 not supported"
            (P.ImageRGBA16 _) ->
                error "ImageRGBA16 not supported"

            (P.ImageY8 (P.Image imgWidth imgHeight dat)) ->
                V.unsafeWith dat $ \ptr -> do
                    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R8
                        (GL.TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0
                        (GL.PixelData GL.Red GL.UnsignedByte ptr)
                    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
            (P.ImageYA8 _) -> error "ImageYA8 not supported"
            (P.ImageYCbCr8 _) -> error "ImageYCbCr8 not supported"
            _ -> error (show "Only RGBA8 supported")

        return $ FontRenderer
            { _frTextAtlas = textAtlas
            , _frAtlasTexture = imageTexture
            , _frAtlasTextureUnit = Just textureUnit
            , _frProgram = program
            , _frVAO = vao
            , _frTopoBuffer = topoBuffer
            , _frColorBuffer = colorBuffer
            , _frElementBuffer = elementBuffer
            , _frAtlasUniform = atlasBuffer
            }

-- | render a text using the font renderer
renderText :: FontRenderer -> Camera -> IO ()
renderText fr cam = do
    GL.currentProgram $= Just (fr^.frProgram)
    programSetViewProjection (fr^.frProgram) cam

    charMapIndex <- GL.getUniformBlockIndex (fr^.frProgram) "CharMap"
    logGL "renderText: getUniformBlockIndex"
    GL.bindBufferBase' GL.UniformBuffer charMapIndex (fr^.frAtlasUniform)
    logGL "renderText: uniform block bind buffer base'"
    GL.uniformBlockBinding (fr^.frProgram) charMapIndex charMapIndex
    logGL "renderText: uniform block binding"

    sampler <- GL.get $ GL.uniformLocation (fr^.frProgram) ("Texture0")
    logGL "renderText: uniform location"
    let Just textureUnit = fr^.frAtlasTextureUnit
    GL.uniform sampler $= textureUnit
    logGL "renderText: texture unit"

    GL.bindVertexArrayObject $= Just (fr^.frVAO)
    logGL "renderText: bindvao"
    GL.bindBuffer GL.ElementArrayBuffer $= Just (fr^.frElementBuffer)
    logGL "renderText: bindElementbuffer"
    --GLRaw.glDrawElements GLRaw.gl_TRIANGLES (fromIntegral $ renderer^.trNumVertices) GLRaw.gl_UNSIGNED_INT nullPtr
    GLRaw.glDrawElements GLRaw.gl_TRIANGLES 12 GLRaw.gl_UNSIGNED_INT nullPtr
    logGL "renderText: glDrawElements"

