{-# LANGUAGE GADTs #-}
import           Control.Monad
import           Graphics.Rendering.OpenGL hiding (bitmap)
import           Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelStorage
import           Graphics.Rendering.FreeType.Internal
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import           Graphics.Rendering.FreeType.Internal.Library
import           Graphics.Rendering.FreeType.Internal.FaceType
import           Graphics.Rendering.FreeType.Internal.Face
import           Graphics.Rendering.FreeType.Internal.GlyphSlot
import           Foreign
import           Foreign.C.String
import           Graphics.Rendering.FreeType.Internal.Bitmap
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS
import           Data.Char
import           System.IO (hPutStrLn, stderr)
import           System.Exit (exitFailure)
import qualified Data.ByteString as B
import Control.Monad                (when, unless)
import Data.Vector.Storable         (unsafeWith, Storable)
import Data.Maybe                   (isNothing)
import Codec.Picture


initTexture :: FilePath -- ^ The texture to load.
            -> Int      -- ^ The index of the texture unit to hold the texture in.
            -> IO (Maybe TextureObject)
initTexture file u = do
    texture Texture2D $= Enabled
    -- Load our texture or die.
    mTex <- loadTexture file u
    unless (isNothing mTex) $ do
        -- Set the texture params on our bound texture.
        textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
        textureWrapMode Texture2D S $= (Repeated, Clamp)
        textureWrapMode Texture2D T $= (Repeated, Clamp)
    when (isNothing mTex) $ putStrLn $ "Could not initialize "++ file
    return mTex


loadTexture :: FilePath -> Int -> IO (Maybe TextureObject)
loadTexture f u = do
    putStrLn $ "Loading texture "++f
    eDynImg <- readImage f
    case eDynImg of
        Left note  -> do
            putStrLn $ "Could not load texture '"++f++"'.\nNote: "++note
            return Nothing

        Right img -> do
            -- Get our texture object.
            tex  <- newBoundTexUnit u
            -- Buffer our texture data.
            success <- bufferDataIntoBoundTexture img
            unless success $ putStrLn $ "    ("++f++")"
            return $ Just tex


newBoundTexUnit :: Int -> IO TextureObject
newBoundTexUnit u = do
    [tex] <- genObjectNames 1
    texture Texture2D $= Enabled
    activeTexture     $= TextureUnit (fromIntegral u)
    textureBinding Texture2D $= Just tex
    return tex


bufferDataIntoBoundTexture :: DynamicImage -> IO Bool
bufferDataIntoBoundTexture dynImg = case dynImg of
    (ImageRGB8 img)  -> unsafeTexImage2D RGB8 RGB img
    (ImageRGBA8 img) -> unsafeTexImage2D RGBA8 RGBA img
    _                -> do
        putStrLn "Texture is not an expected format (expecting RGB8 or RGBA8)."
        return False


unsafeTexImage2D :: (Storable t1, PixelBaseComponent t ~ t1)
                 => PixelInternalFormat
                 -> PixelFormat
                 -> Image t
                 -> IO Bool
unsafeTexImage2D rb r (Image w h dat) = do
    unsafeWith dat $ \ptr ->
        texImage2D
          Texture2D
          -- No proxy
          NoProxy
          -- No mipmaps
          0
          -- Internal storage @ rgba8
          rb
          -- Size of the image
          (TextureSize2D (fromIntegral w) (fromIntegral h))
          -- No borders
          0
          -- Pixel data in unsigned bytes, rgba order
          (PixelData r UnsignedByte ptr)
    printError
    return True

main = return ()


loadCharacter :: FilePath -> Char -> Int -> Int -> IO TextureObject
loadCharacter path char px texUnit = do
    -- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
    ft <- freeType

    -- Get the Ubuntu Mono fontface.
    ff <- fontFace ft path
    runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0

    -- Get the unicode char index.
    chNdx <- ft_Get_Char_Index ff $ fromIntegral $ fromEnum char

    -- Load the glyph into freetype memory.
    runFreeType $ ft_Load_Glyph ff chNdx 0

    -- Get the GlyphSlot.
    slot <- peek $ glyph ff

    -- Number of glyphs
    n <- peek $ num_glyphs ff
    putStrLn $ "glyphs:" ++ show n

    fmt <- peek $ format slot
    putStrLn $ "glyph format:" ++ glyphFormatString fmt

    -- This is [] for Ubuntu Mono, but I'm guessing for bitmap
    -- fonts this would be populated with the different font
    -- sizes.
    putStr "Sizes:"
    numSizes <- peek $ num_fixed_sizes ff
    sizesPtr <- peek $ available_sizes ff
    sizes <- forM [0 .. numSizes-1] $ \i ->
        peek $ sizesPtr `plusPtr` fromIntegral i :: IO BS.FT_Bitmap_Size
    print sizes

    l <- peek $ bitmap_left slot
    t <- peek $ bitmap_top slot
    putStrLn $ concat [ "left:"
                      , show l
                      , "\ntop:"
                      , show t
                      ]

    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

    -- Get the char bitmap.
    bmp <- peek $ bitmap slot
    putStrLn $ concat ["width:"
                      , show $ width bmp
                      , " rows:"
                      , show $ rows bmp
                      , " pitch:"
                      , show $ pitch bmp
                      , " num_grays:"
                      , show $ num_grays bmp
                      , " pixel_mode:"
                      , show $ pixel_mode bmp
                      , " palette_mode:"
                      , show $ palette_mode bmp
                      ]

    let w  = fromIntegral $ width bmp
        h  = fromIntegral $ rows bmp
        w' = fromIntegral w
        h' = fromIntegral h

    -- Set the texture params on our bound texture.
    texture Texture2D $= Enabled

    -- Set the alignment to 1 byte.
    rowAlignment Unpack $= 1

    -- Generate an opengl texture.
    tex <- newBoundTexUnit texUnit
    printError

    putStrLn "Buffering glyph bitmap into texture."
    texImage2D
        Texture2D
        NoProxy
        0
        R8
        (TextureSize2D w' h')
        0
        (PixelData Red UnsignedByte $ buffer bmp)
    printError

    putStrLn "Texture loaded."
    textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    return tex


addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt w val xs = a ++ b ++ c
    where a = take w xs
          b = replicate amt val
          c = addPadding amt w val (drop w xs)


glyphFormatString :: FT_Glyph_Format -> String
glyphFormatString fmt
    | fmt == ft_GLYPH_FORMAT_COMPOSITE = "ft_GLYPH_FORMAT_COMPOSITE"
    | fmt == ft_GLYPH_FORMAT_OUTLINE = "ft_GLYPH_FORMAT_OUTLINE"
    | fmt == ft_GLYPH_FORMAT_PLOTTER = "ft_GLYPH_FORMAT_PLOTTER"
    | fmt == ft_GLYPH_FORMAT_BITMAP = "ft_GLYPH_FORMAT_BITMAP"
    | otherwise = "ft_GLYPH_FORMAT_NONE"


runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r


freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p


fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr

printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)


makeShader :: ShaderType -> B.ByteString -> IO Shader
makeShader ty src = do
    s <- createShader ty
    shaderSourceBS s $= src
    compileShader s
    s'Ok <- get $ compileStatus s
    unless s'Ok $ do
        slog <- get $ shaderInfoLog s
        putStrLn $ "Log:" ++ slog
        exitFailure
    printError
    return s


makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
    p <- createProgram
    mapM_ (attachShader p) shaders
    mapM_ (\(name, loc) -> attribLocation p name $= loc) attributes
    linkProgram p
    p'Ok <- get $ linkStatus p
    validateProgram p
    status <- get $ validateStatus p
    unless (p'Ok && status) $ do
        plog <- get $ programInfoLog p
        putStrLn plog
        printError
        exitFailure
    return p


bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer loc $= (ToFloat, dsc)
    vertexAttribArray loc $= Enabled


charIndex :: Char -> Int
charIndex = (\a -> a-33) . fromIntegral . ord


charToIndices :: Char -> [Int]
charToIndices ' '  = []
charToIndices '\n' = []
charToIndices c = i's
    where i's = [ tl, tr, br
                , tl, br, bl
                ]
          ndx = charIndex c
          n_32 :: Double
          n_32= fromIntegral ndx/32.00
          n__ :: Integer
          n__ = floor n_32
          n'  = ndx + fromIntegral n__
          tl  = n'
          tr  = tl+1
          bl  = tl+33
          br  = tl+34


fontUVPairs :: [[GLfloat]]
fontUVPairs = [ map (/32) [x,y] | y <- [0..6], x <- [0..32] ]


charToUVs :: Char -> [GLfloat]
charToUVs ' '  = []
charToUVs '\n' = []
charToUVs c    = concatMap (\n -> fontUVPairs !! fromIntegral n) (charToIndices c)


charToVerts :: Char -> [GLfloat]
charToVerts _ = [ 0, 0
                , 1, 0
                , 1, 1

                , 0, 0
                , 1, 1
                , 0, 1
                ]


stringToUVs :: String -> [GLfloat]
stringToUVs = concatMap charToUVs


stringToVerts :: String -> [Float]
stringToVerts = fst . foldl f acc
    where f (vs,(x,y)) c
              | c == '\n' = (vs, (0, y+1))
              | c == ' '  = (vs, (x+1,y))
              | otherwise = (vs ++ lst x y, (x+1, y))
          acc     = ([], (0.0, 0.0))
          lst x y = [x,y,x+1,y,x+1,y+1,x,y,x+1,y+1,x,y+1]

