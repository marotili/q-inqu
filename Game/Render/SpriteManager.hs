{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Game.Render.SpriteManager
( Sprite
) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import Graphics.Rendering.OpenGL (($=))

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Control.Monad.Primitive
import Control.Monad.ST

import qualified Data.Map as Map
import Control.Lens
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Control.Monad.State

import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
-- | float to word conversion + word32
import Data.Binary.IEEE754
import Data.Word

-- | gl buffer upload
import Game.Render.Core.Render

type SpriteAtlasName = String
type SpriteName = String

-- | todo: maybe the sprite data type can reference the atlas instread of the atlas name

data Sprite = Sprite
    { _spriteName :: SpriteName
    , _spriteAtlasName :: SpriteAtlasName
    , _spriteOffset :: (Int, Int)
    , _spriteSize :: (Int, Int)
    , _spriteOrigin :: (Int, Int)
    }

newSprite :: SpriteName -> String -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Sprite
newSprite = Sprite

data SpriteAtlas = SpriteAtlas
    { _saName :: String
    , _saSprites :: Map.Map SpriteName Sprite
    , _saImage :: Image
    }

newSpriteAtlas :: SpriteAtlasName -> Image -> SpriteAtlas
newSpriteAtlas name = SpriteAtlas name Map.empty

data Image = Image
    { _iSource :: FilePath
    , _iWidth, _iHeight :: Int
    } deriving (Eq, Show)

newImage :: FilePath -> Int -> Int -> Image
newImage = Image


data SpriteManager = SpriteManager
    { _smAtlas :: Map.Map SpriteAtlasName SpriteAtlas
    }

makeLenses ''Sprite
makeLenses ''SpriteAtlas
makeLenses ''Image
makeLenses ''SpriteManager
    
smAddTile :: String -> Sprite -> StateT SpriteManager Identity ()
smAddTile name sprite = do
    mAtlas <- use $ smAtlas.at name
    case mAtlas of
        Nothing -> error "atlas does not exist"
        Just atlas ->
            smAtlas.at name._Just.saSprites.at (sprite^.spriteName) .= Just sprite

instance A.FromJSON (StateT SpriteManager Identity ()) where
  parseJSON (A.Object v) = do
    tileSets <- v A..: "tileset"
    foldM loadTileSet (return ()) tileSets

    where
      loadTileSet worldState (A.Object v) = do
        tsName <- v A..: "name"
        tsFilename <- fmap FP.fromText (v A..: "filename")
        tsBaseDir <- fmap FP.fromText (v A..: "base_directory")
        tsImageWidth <- v A..: "imageWidth"
        tsImageHeight <- v A..: "imageHeight"
        tsTiles <- v A..: "data"

        let Right filename = FP.toText $ tsBaseDir FP.</> tsFilename

        let newWorldState = smAtlas . at tsName .= (Just $ newSpriteAtlas tsName $
              newImage (T.unpack filename) tsImageWidth tsImageHeight
              )

        newWorldState' <- foldM (loadTiles tsName) newWorldState tsTiles
        return (worldState >> newWorldState')

      loadTiles name worldState (A.Object v) = do
        width <- v A..: "width"
        height <- v A..: "height"
        posX <- v A..: "offset_x"
        posY <- v A..: "offset_y"
        originX <- v A..: "origin_x" :: A.Parser Int
        originY <- v A..: "origin_y" :: A.Parser Int
        tileName <- v A..: "tileId"

        return $ worldState >> 
            smAddTile name (newSprite tileName name (posX, posY) (width, height) (fromIntegral originX, fromIntegral originY))

type LoadTileset = B.ByteString

load :: IO B.ByteString
load =
  B.readFile "tileset_compiled.json"

loadComplexTilesets :: B.ByteString -> State SpriteManager ()
loadComplexTilesets input = do
  let Right updateWorld = A.eitherDecode input :: (Either String (State SpriteManager ()))
  updateWorld

data SpriteAtlasGroup = SpriteAtlasGroup
    { _sagTextures :: Map.Map SpriteAtlasName GL.TextureUnit
    }

data SpriteRenderer = SpriteRenderer
    { _srTextures :: SpriteAtlasGroup
    , _frProgram :: GL.Program
    }

data SpriteBufferElement = SpriteBufferElement
    { _sbePosition :: (Float, Float)
    , _sbeOrigin :: (Float, Float)
    , _sbeSize :: (Float, Float)
    , _sbeRotation :: Float
    , _sbeTextureUnit :: Int -- |starts with zero
    }
makeLenses ''SpriteBufferElement
sbeElementSize :: Int
sbeElementSize = 8 -- | 8 entries

sbePack :: (PrimMonad m) => SpriteBufferElement -> VM.MVector (PrimState m) Word32 -> m ()
sbePack element vec = do
    VM.write vec 0 (floatToWord (element^.sbePosition._1))
    VM.write vec 1 (floatToWord (element^.sbePosition._2))
    VM.write vec 2 (floatToWord (element^.sbeOrigin._1))
    VM.write vec 3 (floatToWord (element^.sbeOrigin._2))
    VM.write vec 4 (floatToWord (element^.sbeSize._1))
    VM.write vec 5 (floatToWord (element^.sbeSize._2))
    VM.write vec 6 (floatToWord (element^.sbeRotation))
    VM.write vec 7 (fromIntegral (element^.sbeTextureUnit))

data SpriteBuffer = SpriteBuffer
    { _sbGLBuffer :: GL.BufferObject
    , _sbElementBuffer :: GL.BufferObject
    , _sbGLBufferSize :: Int -- | size in 4 bytes * 5
    , _sbData :: V.Vector Word32 -- | (Position, Origin, Size, Rotation, TextureUnit)
    , _sbElementData :: V.Vector Word32
    , _sbUsedSize :: Int
    }

makeLenses ''SpriteBuffer
sbBufferSize :: Getter SpriteBuffer Int
sbBufferSize = to (V.length . _sbData)

type NumEntities = Int

--newStaticSpriteBuffer :: NumEntities -> IO SpriteBuffer
--newStaticSpriteBuffer numEntities = do
--    return ()

newDynamicSpriteBuffer' :: NumEntities -> SpriteBuffer
newDynamicSpriteBuffer' numEntities = SpriteBuffer
        { _sbGLBufferSize = numEntities
        , _sbData = emptyVec
        , _sbElementData = emptyElementVec
        , _sbUsedSize = 0
        }
    where
        emptyVec = V.replicate (numEntities*sbeElementSize) 0
        emptyElementVec = V.replicate numEntities 0 


newDynamicSpriteBuffer :: NumEntities -> IO SpriteBuffer
newDynamicSpriteBuffer numEntities = do
    [buffer, elementBuffer] <- GL.genObjectNames 2 :: IO [GL.BufferObject]

    let emptyVec = V.replicate (numEntities*sbeElementSize) 0
    let emptyElementVec = V.replicate numEntities 0

    uploadFromVec (numEntities*sbeElementSize) GL.ArrayBuffer buffer emptyVec
    uploadFromVec numEntities GL.ElementArrayBuffer elementBuffer emptyElementVec

    return SpriteBuffer
        { _sbGLBuffer = buffer
        , _sbElementBuffer = elementBuffer
        , _sbGLBufferSize = numEntities
        , _sbData = emptyVec
        , _sbElementData = emptyElementVec
        , _sbUsedSize = 0
        }

-- | for now we copy the entire data (we receive it sorted by the y-axis)
updateSpriteBuffer :: [SpriteBufferElement] -> State SpriteBuffer ()
updateSpriteBuffer elements = do
    bufferSize <- use sbBufferSize
    if bufferSize < length elements
        then
            error "no resizing implemented"
        else do
            buffer <- use sbData
            elementBuffer <- use sbElementData

            let newBuffer = runST $ do
                    modBuffer <- V.unsafeThaw buffer
                    mapM_ (\i -> do
                        let buf = VM.slice (i*8) ((i+1)*8) modBuffer
                        sbePack (elements!!i) buf
                        ) [0..length elements - 1]
                    V.unsafeFreeze modBuffer

            sbData .= newBuffer

data SpriteRenderUnit = SpriteRenderUnit
    { _sruMain :: SpriteRenderer
    , _sruVAO :: GL.VertexArrayObject
    , _sruBuffer :: SpriteBuffer
    }
