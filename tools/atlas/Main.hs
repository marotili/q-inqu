{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import Data.Maybe
import Game.Data.Bin
import Control.Lens
import Data.Aeson
import Data.List
import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Graphics.ImageMagick.MagickWand
import Filesystem.Path
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FP
import Text.Regex
import Control.Monad.Trans.Resource


data TileSetData = TileSetData
	{ _tsdFilename :: FP.FilePath
	, _tsdSize :: Int
	} deriving (Show)

--newtype TileSetDataList = [TileSetData]

data TileSet = TileSet
	{ _tsName :: T.Text
	, _tsFilename :: FP.FilePath
	, _tsBaseDirectory :: FP.FilePath
	, _tsData :: [TileSetData]
	} deriving (Show)

data TileSets = TileSets
	{ _tileSet :: TileSet
	} deriving (Show)

makeLenses ''TileSetData
makeLenses ''TileSet
makeLenses ''TileSets

instance FromJSON TileSets where
	parseJSON (Object v) = TileSets <$>
		v .: "tileset"
	parseJSON _ = mzero

instance FromJSON TileSet where
	parseJSON (Object v) = TileSet <$>
		v .: "name" <*>
		v .: "filename" <*> 
		v .: "base_directory" <*>
		v .: "data"
	parseJSON _ = mzero

instance FromJSON TileSetData where
	parseJSON (Object v) = TileSetData <$>
		v .: "filename" <*>
		v .: "size"
	parseJSON _ = mzero

instance FromJSON FP.FilePath where
	parseJSON (String v) = return $ FP.fromText v
	parseJSON _ = mzero

readConfig = do
	fileData <- B.readFile "data/tilesets.json" 
	let p = eitherDecode fileData :: Either String TileSets
	print p
	return p

findOffset :: String -> (Int, Int)
findOffset str = case matches of
		Just [l, r] -> (read l, read r)
		_ -> (0, 0)
	where
		r = mkRegex "Origin geometry: \\+([0-9]+)\\+([0-9]+)"
		matches = matchRegex r str

fillBin :: Bin -> [Rect] -> Maybe PackTree
fillBin _ [] = Nothing

fillBin bin rects = 
	let 
		(first:sortedRects) = sort rects
		Just treeRoot = mkRoot first bin

		finalTree = foldr (\rect mtree -> mtree >>= \tree -> updateTree rect tree mtree) (Just treeRoot) sortedRects
	in finalTree

mkRects :: [(Int, Int)] -> [Rect]
mkRects sizes = rects
	where
		rects = map (\((w, h), rId) -> newRect rId w h) $ zip sizes [0..]

getOffsets :: Bin -> [(Int, Int)] -> [(Int, Int)]
getOffsets bin sizes = let
		rects = mkRects sizes
		tree = fromJust $ fillBin bin rects
		atlas = atlasFromTree tree
	in
		map (\rect -> fromJust $ atlas^.atlasRects . at (rect^.rectId)) rects

trim tileSet = do
	s <- withMagickWandGenesis $ do
		(rks, wands) <- fmap unzip $ sequence [magickWand | _ <- [0..numTiles-1]]
		sizes <- mapM (\(tsData, wand) -> do
				readImage wand $ FP.fromText "data" </> (tileSet^.tsBaseDirectory) </> (tsData^.tsdFilename)
				trimImage wand 10
				str <- identifyImage wand

				let offset = findOffset str

				width <- getImageWidth wand
				height <- getImageHeight wand

				let bSize = tsData^.tsdSize
				let bWidth = (round $ fromIntegral bSize / fromIntegral height * fromIntegral width)
				resizeImage wand bWidth bSize lanczosFilter 1
				finalWidth <- getImageWidth wand

				return (finalWidth, bSize)
			) $ zip (tileSet^.tsData) wands

		let rectOffsets = getOffsets (Bin 1024 1024) sizes

		(_, w) <- magickWand

		pw <- pixelWand
		setColor pw "none"
		newImage w 1024 1024 pw
	
		mapM_ (\((ox, oy), pk, wand) -> do
				compositeImage w wand overCompositeOp ox oy
				release pk
			) $ zip3 rectOffsets rks wands

		writeImage w (Just "data/monsters/compiled/test.png")

		return sizes
	print s

	where
		numTiles = length $ tileSet^.tsData

main = do
	Right p <- readConfig

	trim (p^.tileSet)


	--scaleImages (p^.tileSet)

	--l <- withMagickWandGenesis $ do
	--	(_, w) <- magickWand

	--	pw <- pixelWand
	--	setColor pw "none"
	--	newImage w 1024 1024 pw
	

	--	(tmps, ws) <- magickWand
	--	readImage ws $ FP.fromText "data/monsters/1.png"
	--	trimImage ws 10

	--	str <- identifyImage ws
	--	let offset = findOffset str

	--	compositeImage w ws overCompositeOp 0 0

	--	release tmps


	--	(tmps2, ws2) <- magickWand
	--	readImage ws2 $ FP.fromText "data/monsters/2.png"
	--	trimImage ws2 10

	--	compositeImage w ws2 overCompositeOp 500 500
	--	release tmps2


	--	writeImage w (Just "data/monsters/compiled/test.png")


	--	return (str, offset)
	--print l


scaleImages tileSet =
	mapM_ (\tsData -> do
		withMagickWandGenesis $ do
			(_, w) <- magickWand
			scaleImage w tsData
		) (tileSet^.tsData)
	where
		baseDir = tileSet^.tsBaseDirectory
		scaleImage w tsData = do
			readImage w $ FP.fromText "data" </> baseDir </> (tsData^.tsdFilename)
			width <- getImageWidth w
			height <- getImageWidth w
			resizeImage w (tsData^.tsdSize) (tsData^.tsdSize) lanczosFilter 1
			writeImages w ("data" </> baseDir </> "compiled" </> (tsData^.tsdFilename)) True
			--clearMagickWand w
	