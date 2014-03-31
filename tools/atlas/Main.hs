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

trim :: TileSets -> TileSetCompiled -> TileSet -> IO [TileSetData]
trim root compiledTileSet tileSet = do
	(oldSizes, sizes, offsets, trimOffsets, derivedOldSizes, derivedSizes, derivedTrimOffsets) <- withMagickWandGenesis $ do
		(rks, wands) <- fmap unzip $ sequence [magickWand | _ <- [0..numTiles-1]]
		(oldSizes, sizes, trimOffsets) <- fmap unzip3 $ mapM (\(tsData, wand) -> do
				readImage wand $ FP.fromText "data" </> (compiledTileSet^.tscBaseDirectory) </> (tsData^.tsdFilename)
				trimImage wand 10
				str <- identifyImage wand

				let offset = findOffset str

				width <- getImageWidth wand
				height <- getImageHeight wand

				let bSize = tsData^.tsdSize
				let bWidth = (round $ fromIntegral bSize / fromIntegral height * fromIntegral width)
				resizeImage wand bWidth bSize lanczosFilter 1
				finalWidth <- getImageWidth wand

				return $ traceShow offset $ ((width, height), (finalWidth, bSize), offset)
			) $ zip (compiledTileSet^.tscData) wands

		(derivedRks, derivedWands) <- fmap unzip $ sequence [magickWand | _ <- [0..(length $ tileSet^.tsDerivedData) - 1]]
		(derivedOldSizes, derivedSizes, derivedTrimOffsets) <- fmap unzip3 $ mapM (\(derivedData, wand) -> do
				let Just sourceData = (root^.rootTiles.at (derivedData^.tsddSourceName))
				readImage wand $ FP.fromText "data" </> (compiledTileSet^.tscBaseDirectory) </> (sourceData^.tsdFilename)
				trimImage wand 10
				str <- identifyImage wand
				let offset = findOffset str

				width <- getImageWidth wand
				height <- getImageHeight wand

				let bSize = sourceData^.tsdSize
				let bWidth = (round $ fromIntegral bSize / fromIntegral height * fromIntegral width)
				resizeImage wand bWidth bSize lanczosFilter 1
				finalWidth <- getImageWidth wand


				case (derivedData^.tsddTransformation) of
					TransFlipHorizontal -> do
						flopImage wand
					TransFlipVertical -> do
						flipImage wand
					_ -> return ()

				return ((width, height), (finalWidth, bSize), offset)

			) $ zip (tileSet^.tsDerivedData) derivedWands

		let rectOffsets = getOffsets (Bin 4096 4096) (sizes ++ derivedSizes)

		(pk, w) <- magickWand

		pw <- pixelWand
		setColor pw "none"
		newImage w 4096 4096 pw
	
		mapM_ (\((ox, oy), pk, wand) -> do
				compositeImage w wand overCompositeOp ox oy
				release pk
			) $ zip3 rectOffsets (rks ++ derivedRks) (wands ++ derivedWands)

		--mapM_ (\((ox, oy), pk, wand) -> do
		--		compositeImage w wand overCompositeOp ox oy
		--		release pk
		--	) $ zip3 rectOffsetsDerived derivedRks derivedWands

		writeImage w (Just $ "data" </> (compiledTileSet^.tscBaseDirectory) </> "compiled/tileset.png")
		release pk

		return (oldSizes, sizes, rectOffsets, trimOffsets, derivedOldSizes, derivedSizes, derivedTrimOffsets)

	let derivedOffsets = drop (numTiles) offsets
	let ts' = foldr (\(tsData, oldSize@(ow,oh), size@(w,h), trimOffset, offset) ts -> (tsData 
			& tsdFinalSize .~ size
			& tsdBaseOffset .~ trimOffset
			& tsdPosition .~ offset
			& tsdOrigin %~ \(ox, oy) -> let
				wFactor :: Float
				wFactor = (fromIntegral w/fromIntegral ow)
				hFactor :: Float
				hFactor = (fromIntegral h/fromIntegral oh)
					in ( round $ (fromIntegral ox - fromIntegral (trimOffset^._1))*wFactor
						, round $ (fromIntegral oy - fromIntegral (trimOffset^._2))*hFactor
						)
			)
			: ts)
		[] $ zip5 (compiledTileSet^.tscData) oldSizes sizes trimOffsets offsets

	let ts'' = foldr (\(derivedData, oldSize@(ow,oh), size@(w,h), trimOffset, offset) ts -> 
			let Just sourceData = (root^.rootTiles.at (derivedData^.tsddSourceName)) in
				(sourceData 
					& tsdName .~ (derivedData^.tsddName)
					& tsdFinalSize .~ size
					& tsdBaseOffset .~ trimOffset
					& tsdPosition .~ offset
					& tsdOrigin %~ \(ox, oy) -> let
						wFactor :: Float
						wFactor = (fromIntegral w/fromIntegral ow)
						hFactor :: Float
						hFactor = (fromIntegral h/fromIntegral oh)
							in ( round $ (fromIntegral ox - fromIntegral (trimOffset^._1))*wFactor
								, round $ (fromIntegral oy - fromIntegral (trimOffset^._2))*hFactor
								)			
					)
					: ts)
			ts' $ zip5 (tileSet^.tsDerivedData) (derivedOldSizes) (derivedSizes) derivedTrimOffsets derivedOffsets

	return ts''

	where
		numTiles = length $ compiledTileSet^.tscData

main = do
	Right p <- readConfig

	print p	

	p' <- foldM (\root tsc -> do
			ts' <- trim p tsc (fromJust $ p^.tileSets.at (tsc^.tscName))
			let r = foldr (\tile root' ->
					let 
						Just ts = root'^.tileSets . at (tsc^.tscName)
						root2 = if not $ Set.member (tile^.tsdName) (ts^.tsData)
							then
								root' & tileSets.at (tsc^.tscName) .~ 
									(Just $ ts & tsData %~ Set.insert (tile^.tsdName)
										)
							else
								root'
					in root2 & rootTiles . at (tile^.tsdName) L..~ Just tile
				) root ts'
			return r
		) p $ (p^.getCompiledTileSets)

	B.writeFile "tileset_compiled.json" (encodePretty p')

