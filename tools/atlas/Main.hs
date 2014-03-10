{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
module Main where

import Data.Maybe
import Debug.Trace
import Game.Data.Bin
import Control.Lens
import qualified Control.Lens as L
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson as A
import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Graphics.ImageMagick.MagickWand
import Filesystem.Path
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FP
import Text.Regex
import Control.Monad.Trans.Resource
import qualified Data.Map as Map
import qualified Data.Set as Set
import Game.Data.Tileset

readConfig = do
	fileData <- B.readFile "data/tilesets.json" 
	let p = eitherDecode fileData :: Either String TileSets
	print p
	return p

trim tileSet = do
	(sizes, offsets, trimOffsets) <- withMagickWandGenesis $ do
		(rks, wands) <- fmap unzip $ sequence [magickWand | _ <- [0..numTiles-1]]
		(sizes, trimOffsets) <- fmap unzip $ mapM (\(tsData, wand) -> do
				readImage wand $ FP.fromText "data" </> (tileSet^.tscBaseDirectory) </> (tsData^.tsdFilename)
				trimImage wand 10
				str <- identifyImage wand

				let offset = findOffset str

				width <- getImageWidth wand
				height <- getImageHeight wand

				let bSize = tsData^.tsdSize
				let bWidth = (round $ fromIntegral bSize / fromIntegral height * fromIntegral width)
				resizeImage wand bWidth bSize lanczosFilter 1
				finalWidth <- getImageWidth wand

				return ((finalWidth, bSize), offset)
			) $ zip (tileSet^.tscData) wands

		let rectOffsets = getOffsets (Bin 1024 1024) sizes

		(_, w) <- magickWand

		pw <- pixelWand
		setColor pw "none"
		newImage w 1024 1024 pw
	
		mapM_ (\((ox, oy), pk, wand) -> do
				compositeImage w wand overCompositeOp ox oy
				release pk
			) $ zip3 rectOffsets rks wands

		writeImage w (Just "data/monsters/compiled/tileset.png")

		return (sizes, rectOffsets, trimOffsets)

	let ts' = foldr (\(tsData, size, trimOffset, offset) ts -> (tsData 
			& tsdFinalSize .~ size
			& tsdBaseOffset .~ trimOffset
			& tsdPosition .~ offset)
			: ts)
		[] $ zip4 (tileSet^.tscData) sizes trimOffsets offsets

	return ts'

	where
		numTiles = length $ tileSet^.tscData

main = do
	Right p <- readConfig

	p' <- foldM (\root ts -> do
			ts' <- trim ts
			let r = foldr (\tile root' ->
					root' & rootTiles . at (tile^.tsdName) L..~ Just tile
				) root ts'
			return r
		) p $ p^.getCompiledTileSets

	B.writeFile "tileset_compiled.json" (encodePretty p')


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


--scaleImages tileSet =
--	mapM_ (\tsData -> do
--		withMagickWandGenesis $ do
--			(_, w) <- magickWand
--			scaleImage w tsData
--		) (tileSet^.tsData)
--	where
--		baseDir = tileSet^.tsBaseDirectory
--		scaleImage w tsData = do
--			readImage w $ FP.fromText "data" </> baseDir </> (tsData^.tsdFilename)
--			width <- getImageWidth w
--			height <- getImageWidth w
--			resizeImage w (tsData^.tsdSize) (tsData^.tsdSize) lanczosFilter 1
--			writeImages w ("data" </> baseDir </> "compiled" </> (tsData^.tsdFilename)) True
--			--clearMagickWand w
--	