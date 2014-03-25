{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings #-}
module Game.Data.Tileset where

import Data.Maybe
import Debug.Trace
import Game.Data.Bin
import Control.Lens
import qualified Control.Lens as L
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson as A
import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Filesystem.Path
import qualified Filesystem.Path as FP
import qualified Filesystem.Path.CurrentOS as FP
import Text.Regex
import Control.Monad.Trans.Resource
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V

data Boundary = Boundary
	{ _boundaryName :: T.Text
	, _boundaryPolyline :: [(Int, Int)]
	} deriving (Show)

data Group = Group
	{ _groupName :: T.Text
	, _groupOrigin :: (Int, Int)
	, _groupBoundaries :: Map.Map T.Text Boundary
	} deriving (Show)

data TileSetData = TileSetData
	{ _tsdFilename :: FP.FilePath
	, _tsdName :: T.Text
	, _tsdSize :: Int
	, _tsdBaseOffset :: (Int, Int)
	, _tsdFinalSize :: (Int, Int)
	, _tsdPosition :: (Int, Int)
	, _tsdOrigin :: (Int, Int)
	, _tsdGroupName :: Maybe T.Text
	, _tsdBoundaries :: Map.Map T.Text Boundary
	} deriving (Show)

data Transformation = 
	  TransFlipHorizontal
	| TransFlipVertical
	| TransOffset
	| TransEmpty
	deriving (Show)

data TileSetDerivedData = TileSetDerivedData
	{ _tsddName :: T.Text
	, _tsddSourceName :: T.Text
	, _tsddTransformation :: Transformation
	} deriving (Show)

data TsObject = TsObject
	{ _objName :: T.Text
	, _objDesc :: T.Text
	} deriving (Show)

data Animation = Animation
	{ _animName :: T.Text 
	} deriving (Show)

data Direction = DirLeft | DirRight | DirFront | DirBack
	 deriving (Show)

data AnimSeqData = AnimSeqData
	{ _asdTileName :: T.Text
	, _asdTime :: Int
	} deriving (Show)

data AnimSequence = AnimSequence
	{ _asDir :: Direction
	, _asData :: [AnimSeqData]
	} deriving (Show)

data ObjectAnimation = ObjectAnimation
	{ _oaObjName :: T.Text
	, _oaAnimName :: T.Text
	, _oaSequence :: [AnimSequence]
	} deriving (Show)

data TileSet = TileSet
	{ _tsName :: T.Text
	, _tsFilename :: FP.FilePath
	, _tsBaseDirectory :: FP.FilePath
	, _tsData :: Set.Set TileId
	, _tsGroups :: Map.Map T.Text Group
	, _tsDerivedData :: [TileSetDerivedData]
	} deriving (Show)

data TileSetCompiled = TileSetCompiled
	{ _tscName :: T.Text
	, _tscFilename :: FP.FilePath
	, _tscBaseDirectory :: FP.FilePath
	, _tscData :: [TileSetData]
	}

type AnimationId = T.Text
type ObjectId = T.Text
type TileId = T.Text
data TileSets = TileSets
	{ _tileSets :: Map.Map T.Text TileSet
	, _rootTiles :: Map.Map TileId TileSetData
	, _rootObjects :: Map.Map ObjectId TsObject
	, _rootAnimations :: Map.Map AnimationId Animation
	, _rootObjAnims :: Map.Map (ObjectId, AnimationId) ObjectAnimation
	} deriving (Show)

makeLenses ''Boundary
makeLenses ''Group
makeLenses ''TileSetData
makeLenses ''TileSet
makeLenses ''TileSets
makeLenses ''ObjectAnimation
makeLenses ''AnimSequence
makeLenses ''AnimSeqData
makeLenses ''TsObject
makeLenses ''TileSetDerivedData
makeLenses ''TileSetCompiled

getCompiledTileSets :: Getter TileSets [TileSetCompiled]
getCompiledTileSets = to get
	where
		get ts = map (\ts' -> TileSetCompiled
				{ _tscFilename = ts'^.tsFilename
				, _tscName = ts'^.tsName
				, _tscBaseDirectory = ts'^.tsBaseDirectory
				, _tscData = map 
					(\tileName -> fromJust $ ts^.rootTiles.at tileName) $ 
						Set.toList (ts'^.tsData)
				}
			) (Map.elems $ ts^.tileSets)

instance ToJSON TsObject where
	toJSON obj = object ["name" A..= (obj^.objName)]
--instance ToJSON Animation where
--	toJSON obj = object ["name" A..= (obj^.objName)]
--instance ToJSON TsObject where
--	toJSON obj = object ["name" A..= (obj^.objName)]
instance ToJSON TileSets where
	toJSON ts = object
		[ "tileset" A..= (ts^.getCompiledTileSets)
		, "objects" A..= (ts^.rootObjects)
		--, "animations" A..= (ts^.rootAnimations)
		--, "objectAnimations" A..= (ts^.rootObjAnims)
		]

instance ToJSON TileSetCompiled where
	toJSON ts =
		object 
			[ "name" A..= (ts^.tscName)
			, "filename" A..= filename
			, "imageWidth" A..= (4096 :: Int)
			, "imageHeight" A..= (4096 :: Int)
			, "base_directory" A..= baseDir
			, "data" A..= (ts^.tscData)
			]
		where 
			Right filename = FP.toText $ ts^.tscFilename
			Right baseDir = FP.toText $ ts^.tscBaseDirectory

instance ToJSON TileSetData where
	toJSON tsd =
		object 
			[ "tileId" A..= (tsd^.tsdName)
			, "offset_x" A..= (tsd^.tsdPosition._1)
			, "offset_y" A..= (tsd^.tsdPosition._2)
			, "width" A..= (tsd^.tsdFinalSize._1)
			, "height" A..= (tsd^.tsdFinalSize._2)
			]

instance FromJSON TileSets where
	parseJSON (Object v) = do
		let ts = TileSets Map.empty Map.empty Map.empty Map.empty Map.empty
		ts' <- execStateT (do
				parseTileSets (Object v)			
				parseObjects (Object v)
				parseAnimations (Object v)
				parseObjectAnimations (Object v)
			) ts

		return $ ts'

		where
			parseObjects (Object v) = do
				objs <- lift $ v .: "objects"
				mapM (\(Object v) -> do
						obj <- lift $ parseJSON (Object v)
						existingObj <- use $ rootObjects . at (obj^.objName)
						case existingObj of
							Just _ -> error "object name not unique"
							Nothing -> return ()

						rootObjects.at (obj^.objName) L..= Just obj
					) objs

			parseAnimations (Object v) = do
				animations <- lift $ v .: "animations"
				mapM (\(String animName) -> do
						existingAnim <- use $ rootAnimations . at animName
						case existingAnim of
							Just _ -> error "animation name not unique"
							Nothing -> return ()

						rootAnimations . at animName L..= Just (Animation animName)
					) animations

			parseObjectAnimations  (Object v) = do
				objAnims <- lift $ v .: "objectAnimations"
				mapM (\(Object v) -> do
						objAnim <- fmap (\a -> a ([]::[AnimSequence])) $ lift $ parseJSON (Object v)
						existingObj <- use $ rootObjects . at (objAnim^.oaObjName)
						existingAnim <- use $ rootAnimations . at (objAnim^.oaAnimName)
						case existingObj of Nothing -> error "object for animation does not exist"; _ -> return ()
						case existingAnim of Nothing -> error "animation for animation does not exist"; _ -> return ()

						rootObjAnims . at (objAnim^.oaObjName, objAnim^.oaAnimName) L..= Just objAnim

						seqData <- lift $ v .: "sequence"
						mapM_ (parseSequence objAnim) seqData
					) objAnims

			parseSequence objAnim (Object v) = do
				seqData <- lift . parseJSON $ (Object v)

				mapM_ (\seqD -> do
						existingTile <- use $ rootTiles . at (seqD^.asdTileName)
						case existingTile of Nothing -> error "tile does not exist"; _ -> return ()
					) (seqData^.asData) 

				rootObjAnims . at (objAnim^.oaObjName, objAnim^.oaAnimName) . _Just . oaSequence %= (:) seqData

			parseTileSets (Object v) = do
				tileSets <- lift $ v .: "tileset"
				mapM_ parseTileSet tileSets

			parseTileSet (Object v) =
				do
					tsHead <- fmap (\a -> a (Set.empty::Set.Set TileId) (Map.empty::Map.Map T.Text Group) ([]::[TileSetDerivedData])) $
						lift $ parseJSON (Object v)	
					existingTs <- use $ tileSets . at (tsHead^.tsName)
					case existingTs of
						Just _ -> error "tilset name should be unique"; _ -> return ()
						Nothing -> tileSets . at (tsHead^.tsName) L..= Just tsHead

					tileSets . at (tsHead^.tsName) L..= Just tsHead

					tsGroup' <- lift $ v .:? "group_data"
					tsData <- lift $ v.: "source_data"
					tsDerivedData <- lift $ v.: "derived_data"

					case tsGroup' of
						Just tsGroup -> do
							let groupNames = HashMap.keys tsGroup

							mapM_ (\name -> do
									Object group <- lift $ tsGroup .: name
									origin <- lift $ group .: "origin" 
									Object boundaries <- lift $ group .: "boundaries"

									tileSets.at (tsHead^.tsName)._Just.
										tsGroups.at name L..= (Just $ Group name (origin!!0, origin!!1) Map.empty)

									mapM_ (\boundaryName -> do
											polyLine <- lift $ boundaries .: boundaryName
											tileSets.at (tsHead^.tsName)._Just.
												tsGroups.at name._Just.groupBoundaries.at boundaryName L..=
													(Just $ Boundary boundaryName $ foldr (\[x, y] ls -> (x,y):ls) [] polyLine)
										) $ HashMap.keys boundaries
								) groupNames
						_ -> return ()

					mapM_ (parseTsData (tsHead^.tsName)) tsData
					mapM_ (parseDerivedData (tsHead^.tsName)) tsDerivedData

			parseTsData tileSetName (Object v) = do
				tileData' <- lift $ parseJSON (Object v)
				existingTsData <- use $ rootTiles . at (tileData'^.tsdName)

				tileData <- case tileData' ^. tsdGroupName of
					Just groupName -> do
						Just ts <- use $ tileSets.at (tileSetName)
						let Just group = ts^.tsGroups.at groupName

						return $ tileData'
							& tsdOrigin .~ (group^.groupOrigin)
							& tsdBoundaries .~ (group^.groupBoundaries)
					Nothing ->
						return tileData'

				case existingTsData of
					Just _ -> error "tileIds must be unique"
					Nothing -> do
						tileSets . at (tileSetName) . _Just . tsData %= Set.insert (tileData^.tsdName)
						rootTiles . at (tileData^.tsdName) L..= Just tileData

			parseDerivedData tileSetName (Object v) = do
				derivedData <- lift $ parseJSON (Object v)
				existingTile <- use $ rootTiles . at (derivedData^.tsddName)
				existingSourceTile <- use $ rootTiles . at (derivedData^.tsddSourceName)

				case existingTile of
					Just _ -> error "tileIds must be unique (derived)"
					_ -> return ()

				case existingSourceTile of
					Nothing -> error "source tile doesn't exist"
					_ -> return ()

				tileSets . at (tileSetName) . _Just . tsDerivedData %= (:) derivedData
				--rootTiles . at (derivedData^.tsddName) L..= Just derivedData

instance FromJSON (Set.Set TileId -> Map.Map T.Text Group -> [TileSetDerivedData] -> TileSet) where
	parseJSON (Object v) = TileSet <$>
		v .: "name" <*>
		v .: "filename" <*> 
		v .: "base_directory"
	parseJSON _ = mzero

instance FromJSON ([AnimSequence] -> ObjectAnimation) where
	parseJSON (Object v) = ObjectAnimation <$>
		v .: "objectId" <*>
		v .: "animationId"
	parseJSON _ = mzero
--instance FromJSON TileSets where
--	parseJSON (Object v) = TileSets <$>
--		v .: "tileset" <*>
--		v .: "objects" <*>
--		v .: "animationIds" <*>
--		v .: "objectAnimations"
--	parseJSON _ = mzero

instance FromJSON TileSetDerivedData where
	parseJSON (Object v) = TileSetDerivedData <$>
		v .: "tileId" <*>
		v .: "sourceId" <*>
		v .: "transform"
	parseJSON _ = mzero

instance FromJSON Transformation where
	parseJSON (String v) = case v of
		"flipHorizontal" -> return TransFlipHorizontal
		"flipVertical" -> return TransFlipVertical
		_ -> return TransEmpty
	parseJSON _ = mzero

instance FromJSON TsObject where
	parseJSON (Object v) = TsObject <$>
		v .: "objectId" <*>
		v .: "objectDesc"
	parseJSON _ = mzero

instance FromJSON Animation where
	parseJSON (String v) = return $ Animation v
	parseJSON _ = mzero

instance FromJSON AnimSequence where
	parseJSON (Object v) = AnimSequence <$>
		v .: "direction" <*>
		v .: "data"
	parseJSON _ = mzero

instance FromJSON AnimSeqData where
	parseJSON (Object v) = AnimSeqData <$>
		v .: "tileId" <*>
		v .: "time"
	parseJSON _ = mzero

instance FromJSON Direction where
	parseJSON (String v) = return $ case v of
		"Left" -> DirLeft
		"Right" -> DirRight
		"Front" -> DirFront
		"Back" -> DirBack
	parseJSON _ = mzero


instance FromJSON TileSetData where
	parseJSON (Object v) = TileSetData <$>
		v .: "filename" <*>
		v .: "tileId" <*>
		v .: "size" <*> 
		pure (0, 0) <*>  -- base offset
		pure (0, 0) <*>  -- final size
		pure (0, 0) <*>  -- position
		test v
			<*>
		v .:? "group" <*> -- group name
		pure Map.empty -- boundaries

		where
			test v = do
				origin <- v .:? "origin" :: Parser (Maybe [Int])
				case origin of
					Just [x, y] -> pure (x, y)
					_ -> do
						originX <- v.:? "originX" :: Parser (Maybe Int)
						originY <- v.:? "originY" :: Parser (Maybe Int)
						case originX of
							Just x -> pure (x, fromJust originY)
							_ -> pure (0, 0)
	parseJSON _ = mzero



instance FromJSON FP.FilePath where
	parseJSON (String v) = return $ FP.fromText v
	parseJSON _ = mzero

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
		(first:sortedRects) = reverse . sort $ rects 
		Just treeRoot = mkRoot first bin

		finalTree = foldr (\rect mtree -> mtree >>= \tree -> updateTree rect tree mtree) (Just treeRoot) $ reverse sortedRects
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

