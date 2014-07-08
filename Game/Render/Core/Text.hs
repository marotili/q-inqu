{-# LANGUAGE TemplateHaskell, Rank2Types, ImpredicativeTypes #-}
module Game.Render.Core.Text
(
    newFontManager
    , newFont
    , newFontAtlas
    , TextAtlas
    , atlasImage
)
where

-- | TODO: Resource management (Fonts etc.)

import qualified Filesystem.Path as FP
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.Bitmap
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Graphics.Rendering.FreeType.Internal.Matrix
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr

import Bindings.Harfbuzz.Hb
import Bindings.Harfbuzz.HbFt
import Bindings.Harfbuzz.HbShape

import Codec.Picture

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import qualified Data.Map as Map
import Control.Lens
import Control.Monad
import GHC.Word

type Coords = (Float, Float)

data Rect = Rect Coords Coords

data TextAtlas = TextAtlas
    --{ _glTexture :: GL.Texture
    { _atlasCharGlyphs :: Map.Map Char Glyph
    , _atlasCharCoords :: Map.Map Char (Int, Int)
    , _atlasImage :: DynamicImage
    }

type FontSize = Int
data Font = Font
    { _fontFace :: Ptr FT_Face
    , _fontFile :: String --FP.FilePath
    , _fontSize :: FontSize
    }

data FontManager = FontManager
    { _fmFreetypeLib :: Ptr FT_Library
    }

data Glyph = Glyph
    { _glyphWidth :: Int
    , _glyphHeight :: Int
    , _glyphPitch :: Int
    , _glyphData :: [Word8]
    , _glyphChar :: Char
    }

makeLenses ''Glyph
makeLenses ''TextAtlas
makeLenses ''Font
makeLenses ''FontManager

newFontManager :: IO FontManager
newFontManager = do
    libPtr <- malloc
    ftError <- ft_Init_FreeType libPtr

    return $ FontManager
        { _fmFreetypeLib = libPtr
        }

deleteFontManager :: FontManager -> IO ()
deleteFontManager fontManager = do
    free (fontManager^.fmFreetypeLib)
    return ()

--newFont :: FontManager -> FP.FilePath -> FontSize -> IO Font
newFont :: FontManager -> String -> FontSize -> IO Font
newFont fontManager path fontSize = do
    facePtr <- malloc
    ftLib <- peek (fontManager^.fmFreetypeLib)
    _ <- newFace path facePtr ftLib

    let hrez = 100
    let vrez = 72

    face <- peek facePtr
    ft_Set_Char_Size face 0 (fromIntegral fontSize*64) (hrez*vrez) vrez

    let zero = (0*0x10000)
    let one = (1*0x10000)
    let hrez2 = round (((1.0::Float)/ (100::Float)) * 0x10000) :: FT_Fixed

    let matrix = FT_Matrix hrez2 zero zero one
    alloca $ \matPtr -> do
        poke matPtr matrix
        ft_Set_Transform face matPtr nullPtr


    return $ Font
        { _fontFace = facePtr
        , _fontFile = path
        , _fontSize = fontSize
        }

    where
        newFace filename face lib = withCString filename $ \cstr -> ft_New_Face lib cstr 0 face


newFontAtlas :: Font -> IO TextAtlas
newFontAtlas font = do
    let facePtr = (font^.fontFace)
    face <- peek facePtr

    let fullString = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    hb_lang <- withCString "en" $ \lang -> c'hb_language_from_string lang 2    

    buf <- c'hb_buffer_create
    c'hb_buffer_set_direction buf c'HB_DIRECTION_LTR
    c'hb_buffer_set_script buf c'HB_SCRIPT_COMMON
    c'hb_buffer_set_language buf hb_lang
    withCString fullString $ \text -> c'hb_buffer_add_utf8 buf text (fromIntegral $ length $ fullString) 0 (fromIntegral . length $ fullString)
    hb_font <- peek facePtr >>= \font -> c'hb_ft_font_create font nullFunPtr
    c'hb_shape hb_font buf nullPtr 0

    atlas <- alloca $ \glyphCountPtr -> do
        glyphInfo <- c'hb_buffer_get_glyph_infos buf glyphCountPtr
        --glyphPos <- c'hb_buffer_get_glyph_positions buf glyphCountPtr


        bitmapList <- foldM (\bitmaps i -> do
            codepoint <- fmap c'hb_glyph_info_t'codepoint $ peekElemOff glyphInfo i
            peek facePtr >>= \face -> ft_Load_Glyph face codepoint ft_LOAD_RENDER

            --slotPtr <- peek facePtr >>= \face -> fmap glyph 
            newBitmap <- peek facePtr >>= \face -> do
                let slotPtr = glyph face
                peek slotPtr >>= \slot -> do
                    let bitmapPtr = bitmap slot 
                    peek bitmapPtr >>= \bitmap -> do
                        imgData <- mapM (\(x, y) -> do
                                c8 <- peekElemOff (buffer bitmap) (y*(fromIntegral $ pitch bitmap) + x)
                                return $ BS.c2w . castCCharToChar $ c8    
                            ) [(x, y) | y <- [0..(fromIntegral $ rows bitmap-1)], x <- [0..(fromIntegral $ width bitmap - 1)]]
                        return $ Glyph
                            { _glyphWidth = fromIntegral (width bitmap)
                            , _glyphHeight = fromIntegral (rows bitmap)
                            , _glyphPitch = fromIntegral (pitch bitmap)
                            , _glyphData = imgData
                            , _glyphChar = fullString !! i
                            }
            return (newBitmap:bitmaps) :: IO [Glyph]

            ) [] [0..(length fullString)-1]

        -- simple atlas
        -- TODO: binpacking
        let maxWidth = maximum $ map (\bitmap -> bitmap^.glyphWidth) bitmapList
        let maxHeight = maximum $ map (^.glyphHeight) bitmapList

        let atlasSize = 256
        let bitmapsPerRow = atlasSize `div` maxWidth
        let bitmapsPerColumn = atlasSize `div` maxHeight

        let offsets = take (length fullString) [(x*maxWidth, y*maxHeight) | y <- [0..bitmapsPerColumn-1], x <- [0..bitmapsPerRow-1]]

        let imgData = foldr (\(bitmap, offset) pointMap ->
                foldr (\(x, y) pointMap' -> if x - offset^._1 < (bitmap^.glyphPitch) && y - offset^._2 < (bitmap^.glyphHeight) then
                        Map.insert (x, y) ((bitmap^.glyphData)!!((y - offset^._2)*(fromIntegral $ bitmap^.glyphPitch) + (x - offset^._1))) pointMap'
                    else
                        Map.insert (x, y) (0) pointMap'
                        ) pointMap [(x, y) | x <- [offset^._1..(offset^._1 + maxWidth)], y <- [offset^._2..(offset^._2 + maxHeight)]]
                ) Map.empty (zip bitmapList offsets)

        let img = generateImage (\x y -> if Map.member (x,y) imgData then imgData Map.! (x,y) else 0) atlasSize atlasSize

        let atlas = foldr (\(glyph, offset) atlas ->
                (atlas & atlasCharGlyphs %~ Map.insert (glyph^.glyphChar) glyph
                       & atlasCharCoords %~ Map.insert (glyph^.glyphChar) offset)
                ) (TextAtlas Map.empty Map.empty (ImageY8 img)) (zip bitmapList offsets)

        saveBmpImage ("test" ++ show 0 ++ ".bmp") (ImageY8 img)
        return atlas

    return atlas