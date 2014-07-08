{-# LANGUAGE TemplateHaskell #-}
module Game.Render.Core.Manager 
()
where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.Set as Set
import Game.Render.Core.Text
import Game.Render.Core.Error
import Game.Render.Core.Render
import qualified Codec.Picture as P
import Control.Lens
import Control.Monad.State.Strict
import qualified Data.Vector.Storable as V
import Control.Monad.Morph

data RenderManager = RenderManager
    { _rmUsedTextures :: Set.Set GL.TextureUnit
    }

data FontRenderer = FontRenderer
    { _frTextAtlas :: TextAtlas
    , _frAtlasTexture :: GL.TextureObject
    , _frAtlasTextureUnit :: Maybe GL.TextureUnit
    }

makeLenses ''RenderManager
makeLenses ''FontRenderer

--generalize :: Monad m => Identity a -> m a
--generalize = return . runIdentity

mkTextureUnit :: State RenderManager GL.TextureUnit
mkTextureUnit = do
    usedTextures <- use rmUsedTextures
    let unit = head $ filter (\unit -> not $ Set.member unit usedTextures) . map GL.TextureUnit $ [0..]
    rmUsedTextures %= Set.insert unit
    return unit

newFontRenderer :: TextAtlas -> StateT RenderManager IO FontRenderer
newFontRenderer textAtlas = do
    [imageTexture] <- lift (GL.genObjectNames 1 :: IO [GL.TextureObject])

    textureUnit <- hoist generalize mkTextureUnit

    lift $ do
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
            (P.ImageRGBA16 (P.Image imgWidth imgHeight dat)) -> 
                V.unsafeWith dat $ \ptr -> do
                    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.R8
                        (GL.TextureSize2D (fromIntegral imgWidth) (fromIntegral imgHeight)) 0
                        (GL.PixelData GL.Red GL.UnsignedByte ptr)
                    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)
            (P.ImageY8 _) -> error "ImageY8 not supported"
            (P.ImageYA8 _) -> error "ImageYA8 not supported"
            (P.ImageYCbCr8 _) -> error "ImageYCbCr8 not supported"
            _ -> error (show ("Only RGBA8 supported"))


    return $ FontRenderer
        { _frTextAtlas = textAtlas
        , _frAtlasTexture = imageTexture
        , _frAtlasTextureUnit = Just textureUnit
        }
