{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TemplateHaskell, Rank2Types, NamedFieldPuns #-}
module Game.Render.World where

import Data.List
import qualified Data.Tiled as T
import qualified Game.World.Import.Tiled as T
import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Aeson as A
import qualified Filesystem.Path as FP
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import qualified Game.Data.Tileset as TS

import Control.Lens

-- | TODO: there is a lot of state inside the lens
-- |   maybe we should remove the lens and keep the state

type TileName = String
type TilesetName = String

data RenderConfig = RenderConfig
	{ _rcTiles :: !(Map.Map TileName (TilesetName, Int))
	} deriving (Show, Eq)
emptyConfig = RenderConfig
	{ _rcTiles = Map.empty
	}
makeLenses ''RenderConfig

type TilesetId = Int
type LayerId = Int

type TileId = Int
type LocalTileId = TileId
type TileIdOffset = Int

type ObjectId = Int

type Position = (Float, Float)

data World = World
	{ _mapWidth, _mapHeight :: !Int
	, _mapTileWidth, _mapTileHeight :: !Int
	, _mapTilesets :: !(Map.Map TilesetId Tileset)
	, _mapComplexTilesets :: !(Map.Map TilesetId ComplexTileset)
	, _mapTsOffsets :: !(Map.Map TilesetId TileIdOffset)
	, _mapLayers :: !(Map.Map LayerId MapLayer)
	, _mapObjects :: !(Map.Map ObjectId Object)
	, _mapHashes :: !(ObjHashes)
	--, _mapUpdates :: MapUpdates
	, _mapNextId :: !Int

	, _mapUpdateLayers :: !(Set.Set LayerId)
	
	, _wRenderConfig :: !RenderConfig
	} deriving (Eq, Show)

--data MapUpdates = MapUpdates
--	{ 
--	} deriving (Eq, Show)

data ObjHashes = ObjHashes
	{ _gameObjects :: !(Map.Map String ObjectId)
	, _gamePrefabs :: !(Map.Map String (TilesetId, LocalTileId))
	, _hashLayers :: !(Map.Map String LayerId)
	, _gameTilesets :: !(Map.Map String TilesetId)
	} deriving (Eq, Show)

data TileType = 
	  TileTypeDefault
	| TileTypeWall
	deriving (Eq, Show)

data Image = Image
	{ _iSource :: !FilePath
	, _iWidth, _iHeight :: !Int
	} deriving (Eq, Show)

data ComplexTileset = ComplexTileset
	{ _ctsImage :: !Image
	, _ctsImageSize :: !(Int, Int)
	, _ctsTiles :: !(Map.Map LocalTileId CtsData)
	, _ctsNextLocalId :: LocalTileId
	} deriving (Eq, Show)

data CtsData = CtsData
	{ _ctsdPos :: !(Int, Int)
	, _ctsdSize :: !(Int, Int)
	} deriving (Eq, Show)

data Tileset = Tileset
	{ _tsTileWidth, _tsTileHeight :: !Int
	, _tsSpacing, _tsMargin :: !Int
	, _tsImage :: !Image
	, _tsTileTypes :: !(Map.Map LocalTileId TileType)
	} deriving (Eq, Show)

data Tile = Tile
	{ _tileTsId :: !TilesetId
	, _tileLocalId :: !LocalTileId
	} deriving (Eq, Show)

data Object = Object
	{ _objTsId :: !TilesetId
	, _objLocalId :: !LocalTileId
	} deriving (Eq, Show)

newObject ts lid = Object
	{ _objTsId = ts
	, _objLocalId = lid
	}

newTile ts lid = Tile
	{ _tileTsId = ts
	, _tileLocalId = lid
	}

data RenderObject = RenderObject
	{ _roId :: !ObjectId
	, _roPos :: !Position
	, _roRotation :: !Float
	, _roOrigin :: !Position
	} deriving (Eq, Show)

newRenderObject oId pos rotation = RenderObject
	{ _roId = oId
	, _roPos = pos
	, _roRotation = rotation
	, _roOrigin = (0, 0)
	}

data LayerType = TileLayerType | ObjectLayerType | ComplexLayerType
	deriving (Eq, Show)

data MapLayer = TileLayer
	{ _layerTiles :: !(Map.Map (Int, Int) Tile)
	}
	| ObjectLayer
	{ _layerObjects :: !(Map.Map ObjectId RenderObject)
	}
	| ComplexObjectLayer
	{ _layerObjects :: !(Map.Map ObjectId RenderObject)
	}
	deriving (Eq, Show)

makeLenses ''World
makeLenses ''MapLayer
makeLenses ''Tileset
makeLenses ''Object
makeLenses ''RenderObject
makeLenses ''Tile
makeLenses ''Image
makeLenses ''ObjHashes
makeLenses ''ComplexTileset
makeLenses ''CtsData

newWorld :: World
newWorld = World
	{ _mapWidth = 0
	, _mapHeight = 0
	, _mapTileWidth = 0
	, _mapTileHeight = 0
	, _mapTilesets = Map.empty
	, _mapComplexTilesets = Map.empty
	, _mapTsOffsets = Map.empty
	, _mapLayers = Map.empty
	, _mapObjects = Map.empty
	, _mapHashes = newHashes
	, _mapNextId = 1
	, _mapUpdateLayers = Set.empty
	, _wRenderConfig = emptyConfig
	}

_wId :: State World Int
_wId = do
	id_ <- use mapNextId
	mapNextId += 1
	return id_

wLayerName :: LayerId -> Getter World String
wLayerName lId = to (\w -> fst . head $ 
		filter (\(name, lId') -> lId == lId') $
			Map.toList (w^.mapHashes.hashLayers)
	)

wSize :: Lens' World (Int, Int)
wSize = lens (\w -> (w^.mapWidth, w^.mapHeight))
	(\world (w, h) -> world 
		& mapWidth .~ w
		& mapHeight .~ h
		)

wTileSize :: Lens' World (Int, Int)
wTileSize = lens (\w -> (w^.mapTileWidth, w^.mapTileHeight))
	(\world (w, h) -> world 
		& mapTileWidth .~ w
		& mapTileHeight .~ h
		)

wTile :: String -> Getter World Tile
wTile name = to get
	where get world = newTile tsId localTileId
		where
			tileMap = world^.wRenderConfig . rcTiles
			Just (tilesetName, localTileId) = tileMap ^. at name

			Just tsId = world ^. mapHashes . gameTilesets . at tilesetName

wComplexTileset :: String -> Lens' World (Maybe ComplexTileset)
wComplexTileset name = lens get set
	where
		get world =
			let mtsId = world^.mapHashes.gameTilesets.at name
			in mtsId >>= \tsId -> world^.mapComplexTilesets . at tsId
		set world mts = case mts of
			Just ts -> wUpdate (do
					existingTilesets <- use $ mapHashes . gameTilesets
					if not (Map.member name existingTilesets) then
						wAddComplexTileset name
						else return ()
					Just tsId <- use $ mapHashes.gameTilesets.at name
					mapComplexTilesets.at tsId .= Just ts
				) world
			Nothing -> wUpdate (wRemoveComplexTileset name) world

wTileset :: String -> Lens' World (Maybe Tileset)
wTileset name = lens get set
	where
		get world = 
			let mtsId = world^.mapHashes.gameTilesets.at name
			in mtsId >>= \tsId -> world^.mapTilesets . at tsId
		set world mts = case mts of
			Just ts -> wUpdate (do
					existingTilesets <- use $ mapHashes . gameTilesets
					if not (Map.member name existingTilesets) then
						wAddTileset name
						else return ()
					Just tsId <- use $ mapHashes.gameTilesets.at name
					mapTilesets.at tsId .= Just ts
				) world
			Nothing -> wUpdate (wRemoveTileset name) world

wAddComplexTile :: String -> String -> (Int, Int) -> (Int, Int) -> State World ()
wAddComplexTile tilesetName objName tilePos tileSize = do
	Just tsId <- use $ mapHashes . gameTilesets . at tilesetName
	Just ts <- use $ mapComplexTilesets . at tsId
	let nextId = ts^.ctsNextLocalId
	mapComplexTilesets . at tsId . _Just . ctsNextLocalId += 1
	mapComplexTilesets . at tsId . _Just . ctsTiles . at nextId .= (Just $ CtsData tilePos tileSize)

	--oId <- _wId
	--mapObjects . at oId .= (Just $ newObject tsId nextId)
	mapHashes . gamePrefabs . at objName .= Just (tsId, nextId)

wObjectFromPrefab :: String -> String -> State World ObjectId
wObjectFromPrefab prefabName objName = do
	Just (tsId, localId) <- use $ mapHashes . gamePrefabs . at objName
	wObject objName .= (Just $ newObject tsId localId)
	Just oId <- use $ mapHashes . gameObjects . at objName
	return oId

wAddObject :: String -> State World ()
wAddObject name = do
	oId <- _wId	
	mapObjects . at oId .= (Just $ newObject 0 0)
	mapHashes . gameObjects . at name .= Just oId

wRemoveObject :: String -> State World ()
wRemoveObject name = do
	Just oId <- use $ mapHashes.gameObjects.at name
	mapObjects . at oId .= Nothing
	mapHashes.gameObjects.at name .= Nothing

	--mapM_ (\(lId, layer) -> do
	--	if (layer^.
	--		mapLayers.at lId 
	--	) $ mapLayers.itraversed

wObjectId name = to get
	where
		get world = world^.mapHashes.gameObjects.at name

wObject :: String -> Lens' World (Maybe Object)
wObject name = lens get set
	where
		get world = 
			let moId = world^.mapHashes.gameObjects.at name
			in moId >>= \oId -> world^.mapObjects . at oId
		set world mobj = case mobj of
			Just obj -> wUpdate (do
					existingObjects <- use $ mapHashes . gameObjects
					if not (Map.member name existingObjects) then
						wAddObject name
						else return ()
					Just oId <- use $ mapHashes.gameObjects.at name
					mapObjects.at oId .= Just obj
				) world
			Nothing -> wUpdate (wRemoveObject name) world

wLayerTile :: String -> (Int, Int) -> Lens' World (Maybe Tile)
wLayerTile layerName (x, y) = lens get set
	where
		get world = case world^?wLayer layerName._Just._layerTile (x, y) of
			Just a -> a
			Nothing -> error "failed to find layer tile of layer"
		set world mro = let 
				Just lId = world^.mapHashes.hashLayers.at layerName
			in world 
				& wLayer layerName._Just._layerTile (x, y) .~ mro
				& mapUpdateLayers %~ Set.insert lId

wLayerObject :: String -> String -> Lens' World (Maybe RenderObject)
wLayerObject layerName objName = lens get set
	where
		get world = let Just oId = world^.mapHashes.gameObjects.at objName
			in case world^?wLayer layerName._Just._layerObject oId of
				Just a -> a
				Nothing -> error "failed to find layer object"

		set world mro = let 
				Just oId = world^.mapHashes.gameObjects.at objName
				Just lId = world^.mapHashes.hashLayers.at layerName
			in world 
				& wLayer layerName._Just._layerObject oId .~ mro
				& mapUpdateLayers %~ Set.insert lId

_layerTile :: (Int, Int) -> Lens' MapLayer (Maybe Tile)
_layerTile (x, y) = lens get set
	where 
		get ml = case ml of
			TileLayer { _layerTiles } -> _layerTiles^.at (x, y)
			otherwise -> Nothing
		set ml mro = case ml of
			tl@TileLayer { _layerTiles } -> tl & layerTiles . at (x, y) .~ mro
			otherwise -> ml
			 

_layerObject :: ObjectId -> Lens' MapLayer (Maybe RenderObject)
_layerObject oId = lens get set
	where 
		get ml = case ml of
			ObjectLayer {_layerObjects } -> _layerObjects^.at oId
			ComplexObjectLayer { _layerObjects } -> _layerObjects^.at oId
			otherwise -> Nothing
		set ml mro = case ml of
			ol@ObjectLayer { _layerObjects } -> ol & layerObjects . at oId .~ mro
			ol@ComplexObjectLayer { _layerObjects } -> ol & layerObjects . at oId .~ mro
			otherwise -> ml 

wAddComplexTileset :: String -> State World ()
wAddComplexTileset name = do
	tsId <- _wId
	mapComplexTilesets . at tsId .= Just (newComplexTileset newImage)
	tsTilesets <- use mapComplexTilesets
	-- tiled starts with offset 1
	--let newOffset = 1 + (sum $ map ((^.tsNumTiles) . snd) $ Map.toList tsTilesets)
	--mapTsOffsets . at tsId .= Just newOffset
	mapHashes . gameTilesets . at name .= Just tsId

wRemoveComplexTileset :: String -> State World ()
wRemoveComplexTileset name = do
	Just tsId <- use $ mapHashes.gameTilesets.at name
	mapComplexTilesets . at tsId .= Nothing
	mapHashes.gameTilesets.at name .= Nothing
	-- list of offsets

wAddTileset :: String -> State World ()
wAddTileset name = do
	tsId <- _wId
	mapTilesets . at tsId .= Just newTileset
	tsTilesets <- use mapTilesets
	-- tiled starts with offset 1
	let newOffset = 1 + (sum $ map ((^.tsNumTiles) . snd) $ Map.toList tsTilesets)
	mapTsOffsets . at tsId .= Just newOffset
	mapHashes . gameTilesets . at name .= Just tsId

wRemoveTileset :: String -> State World ()
wRemoveTileset name = do
	Just tsId <- use $ mapHashes.gameTilesets.at name
	mapTsOffsets . at tsId .= Nothing
	mapTilesets . at tsId .= Nothing
	mapHashes.gameTilesets.at name .= Nothing

	tsTilesets <- use mapTilesets
	-- list of offsets
	let offsets = foldr (\ts l -> l ++ [last l + ts^.tsNumTiles]) [1] $ map snd $ Map.toList tsTilesets

	mapM_ (\(tsId, newOffset) ->
			mapTsOffsets . at tsId .= Just newOffset
		) $ zip (map fst $ Map.toList tsTilesets) offsets

wLayer :: String -> Lens' World (Maybe MapLayer)
wLayer name = lens get set
	where
		get world = 
			let mlayerId = world^.mapHashes.hashLayers.at name
			in mlayerId >>= \layerId -> world^.mapLayers . at layerId
		set world mlayer = case mlayer of
			Just layer -> case layer of
				TileLayer {} -> wUpdate (do
						existingLayers <- use $ mapHashes . hashLayers
						if not (Map.member name existingLayers) then
							wAddLayer TileLayerType name
							else return ()
						Just lId <- use $ mapHashes . hashLayers . at name
						mapLayers . at lId .= Just layer
					) world
				ObjectLayer {} -> wUpdate (do
						existingLayers <- use $ mapHashes . hashLayers
						if not (Map.member name existingLayers) then
							wAddLayer ObjectLayerType name
							else return ()
						Just lId <- use $ mapHashes . hashLayers . at name
						mapLayers . at lId .= Just layer
					) world

				ComplexObjectLayer {} -> wUpdate (do
						existingLayers <- use $ mapHashes . hashLayers
						if not (Map.member name existingLayers) then
							wAddLayer ComplexLayerType name
							else return ()

						Just lId <- use $ mapHashes . hashLayers . at name
						mapLayers . at lId .= Just layer
					) world
			Nothing -> wUpdate (wRemoveLayer name) world

wAddLayer :: LayerType -> String -> State World ()
wAddLayer layerType name = do
	layerId <- _wId
	mapLayers . at layerId .= Just (newLayer layerType)
	mapHashes . hashLayers . at name .= Just layerId

wRemoveLayer :: String -> State World ()
wRemoveLayer name = do
	Just layerId <- use $ mapHashes . hashLayers . at name
	mapLayers . at layerId .= Nothing
	mapHashes . hashLayers . at name .= Nothing

wUpdate :: State World () -> World -> World
wUpdate s w = execState s w

newHashes :: ObjHashes
newHashes = ObjHashes
	{ _gameObjects = Map.empty
	, _hashLayers = Map.empty
	, _gameTilesets = Map.empty
	, _gamePrefabs = Map.empty
	}

newImage :: Image
newImage = Image
	{ _iSource = ""
	, _iWidth = 0
	, _iHeight = 0
	}

newComplexTileset :: Image -> ComplexTileset
newComplexTileset image = ComplexTileset
	{ _ctsImage = image
	, _ctsImageSize = (image^.iWidth, image^.iHeight)
	, _ctsTiles = Map.empty
	, _ctsNextLocalId = 0
	}

newTileset :: Tileset
newTileset = Tileset
	{ _tsTileWidth = 0
	, _tsTileHeight = 0
	, _tsSpacing = 0
	, _tsMargin = 0
	, _tsImage = newImage
	, _tsTileTypes = Map.empty}

tsNumTiles :: Getter Tileset Int
tsNumTiles = to numTiles
	where
		numTiles ts = (ts^.tsWidth) * (ts^.tsHeight)

tsWidth :: Getter Tileset Int
tsWidth = to numTiles
	where
		numTiles ts | tileWidth == 0 = 0
					| otherwise = (width `div` tileWidth)
			where
				width = ts^.tsImage.iWidth - ts^.tsMargin + ts^.tsSpacing
				tileWidth = ts^.tsTileWidth + ts^.tsSpacing

tsHeight :: Getter Tileset Int
tsHeight = to numTiles
	where
		numTiles ts | tileHeight == 0 = 0
					| otherwise = (height `div` tileHeight)
			where
				height = ts^.tsImage.iHeight - ts^.tsMargin + ts^.tsSpacing
				tileHeight = ts^.tsTileHeight + ts^.tsSpacing

tsTileSize :: Lens' Tileset (Int, Int)
tsTileSize = lens (\ts -> (ts^.tsTileWidth, ts^.tsTileHeight))
	(\ts (w, h) -> ts 
		& tsTileWidth .~ w
		& tsTileHeight .~ h
		)

newLayer :: LayerType -> MapLayer
newLayer TileLayerType = TileLayer
	{ _layerTiles = Map.empty
	}
newLayer ObjectLayerType = ObjectLayer
	{ _layerObjects = Map.empty
	}
newLayer ComplexLayerType = ComplexObjectLayer
	{ _layerObjects = Map.empty
	}

wImages :: Getter World [(TilesetId, Image)]
wImages = to (\w -> map (\(tId, ts) -> (tId, ts^.tsImage)) $ Map.toList (w^.mapTilesets))

wGetTile :: String -> (Int, Int) -> Getter World TileId
wGetTile tsName (x, y) = to get
	where
		get world = offset + y*(ts^.tsWidth) + x
			where
				Just tsId = world^.mapHashes.gameTilesets.at tsName
				Just ts = world^.mapTilesets.at tsId
				Just offset = world^.mapTsOffsets.at tsId

wObjTileId :: Object -> Getter World TileId
wObjTileId obj = to get
	where
		get world = let Just offset = world^.mapTsOffsets.at (obj^.objTsId) in
			offset + (obj^.objLocalId)

wTileTileId tile = to get
	where
		get world = let Just offset = world^.mapTsOffsets.at (tile^.tileTsId) 
			in offset + (tile^.tileLocalId)

wLayerNumObjects :: String -> Getter World Int
wLayerNumObjects layerName = to get
	where get world = case layer of
				TileLayer { _layerTiles } -> Map.size _layerTiles
				ObjectLayer { _layerObjects } -> Map.size _layerObjects
				ComplexObjectLayer { _layerObjects } -> Map.size _layerObjects
			where
				Just lId = world^.mapHashes.hashLayers.at layerName
				Just layer = world^.mapLayers.at lId

sortY :: RenderObject -> RenderObject -> Ordering
sortY ro1 ro2  
	| y >= y2 = LT -- -y <= -y2
	| otherwise = GT
	where
		(x, y) = ro1^.roPos 
		(x2, y2) = ro2^.roPos

wTileIds :: String -> Getter World [TileId]
wTileIds layerName = to get
	where
		get world = case layer of
				TileLayer { _layerTiles } -> map getTileTileId $ Map.elems _layerTiles
				ObjectLayer { _layerObjects } -> map getObjectTileId $ sortBy sortY $ Map.elems _layerObjects
				ComplexObjectLayer { _layerObjects } -> map (const 0) $ sortBy sortY $ Map.elems _layerObjects
			where
				Just lId = world^.mapHashes.hashLayers.at layerName
				Just layer = world^.mapLayers.at lId

				getObjectTileId ro = 
					let Just obj = (world^.mapObjects.at (ro^.roId))
					in (world^.wObjTileId obj)

				getTileTileId tile = world^.wTileTileId tile

wIsComplexLayer layerId = to get
	where
		get world =
			let Just layer = world^.mapLayers.at layerId
			in case layer of ComplexObjectLayer {} -> True; _ -> False

wTileMesh :: String -> Getter World [(Float, Float, Float, Float, Float, Float, Int)]
wTileMesh layerName = to get
	where
		get world = case layer of
			ComplexObjectLayer { _layerObjects } -> map (\objId ->
					let 
						Just obj = world^.mapObjects.at (objId)
						Just ts = traceShow (obj) $ world^.mapComplexTilesets.at (obj^.objTsId)
						Just pos = ts^?ctsTiles.at (obj^.objLocalId) . _Just . ctsdPos
						Just size = ts^?ctsTiles.at (obj^.objLocalId) . _Just . ctsdSize
						imgSize = ts^.ctsImageSize
						sx = fromIntegral $ size^._1
						sy = fromIntegral $ size^._2
						px = fromIntegral $ pos^._1
						py = fromIntegral $ pos^._2
						imgW = fromIntegral $ imgSize^._1
						imgH = fromIntegral $ imgSize^._2
					in
						(px, py, sx, sy, imgW, imgH, obj^.objTsId)
				) $ map fst $ sortBy sortY2 $ Map.toList _layerObjects
			_ -> []
			where
				Just lId = world^.mapHashes.hashLayers.at layerName
				Just layer = world^.mapLayers.at lId

-- | TODO remove this
sortY2 :: (ObjectId, RenderObject) -> (ObjectId, RenderObject) -> Ordering
sortY2 ro1 ro2  
	| y >= y2 = LT -- -y <= -y2
	| otherwise = GT
	where
		(x, y) = ro1^._2.roPos 
		(x2, y2) = ro2^._2.roPos

wTilePos :: String -> Getter World [(Float, Float, Float, Float, Float)]
wTilePos layerName = to get
	where
		get world = case layer of
			TileLayer { _layerTiles } -> map (\(x, y) ->
					( fromIntegral $ x * world^.mapTileWidth
					, fromIntegral $ y * world^.mapTileHeight
					, 0, 0
					, 0
					)
				) $ Map.keys _layerTiles
			ObjectLayer { _layerObjects } -> map (\obj -> 
					( obj^.roPos._1
					, obj^.roPos._2
					, obj^.roOrigin._1
					, obj^.roOrigin._2
					, obj^.roRotation
					)
				) $ sortBy sortY $ Map.elems _layerObjects
			ComplexObjectLayer { _layerObjects } -> map (\obj -> 
					( obj^.roPos._1
					, obj^.roPos._2
					, obj^.roOrigin._1
					, obj^.roOrigin._2
					, obj^.roRotation
					)
				) $ sortBy sortY $ Map.elems _layerObjects
			where
				Just lId = world^.mapHashes.hashLayers.at layerName
				Just layer = world^.mapLayers.at lId


--mlTileIds :: MapLayer -> [TileId]
--mlCoords :: MapLayer -> [(Float, Float)]

shallowEqual :: World -> World -> Bool
shallowEqual w1 w2 = w && h && tw && th && ts && tsOff && ml && obj && hashs
	where
		w = w1^.mapWidth == w2^.mapWidth
		h = w1^.mapHeight == w2^.mapHeight
		tw = w1^.mapTileWidth == w2^.mapTileWidth
		th = w1^.mapTileHeight == w2^.mapTileHeight
		ts = Map.elems (w1^.mapTilesets) == Map.elems (w2^.mapTilesets)
		tsOff = Map.elems (w1^.mapTsOffsets) == Map.elems (w2^.mapTsOffsets)
		ml = w1^.mapLayers == w2^.mapLayers
		obj = w1^.mapObjects == w2^.mapObjects
		hashs = shallowHash (w1^.mapHashes) (w2^.mapHashes)

		shallowHash h1 h2 = ts && objs && lys
			where
				ts = Map.keys (h1^.gameTilesets) == Map.keys (h2^.gameTilesets)
				objs = Map.keys (h1^.gameObjects) == Map.keys (h2^.gameObjects)
				lys = Map.keys (h1^.hashLayers) == Map.keys (h2^.hashLayers)


loadMapFromTiled :: T.TiledMap -> World
loadMapFromTiled tiledMap = wUpdate (do
		wSize .= (tiledMap^.T.mapWidth, tiledMap^.T.mapHeight)
		wTileSize .= (tiledMap^.T.mapTileWidth, tiledMap^.T.mapTileHeight)
		mapM_ (\ts ->
			wTileset (ts^.T.tsName) .= (Just $ newTileset
				& tsTileSize .~ (ts^.T.tsTileWidth, ts^.T.tsTileHeight)
				& tsSpacing .~ (ts^.T.tsSpacing)
				& tsMargin .~ (ts^.T.tsMargin)
				& tsImage .~ (newImage 
						& iSource .~ (ts^?!T.tsImages._head.T.iSource)
						& iWidth .~ (ts^?!T.tsImages._head.T.iWidth)
						& iHeight .~ (ts^?!T.tsImages._head.T.iHeight)
					)
				& tsTileTypes .~ (foldr (\tId -> Map.insert tId TileTypeWall) Map.empty $ wallIds ts)
				)
			) (tiledMap^.T.mapTilesets)
		) newWorld
	where
		wallIds ts = map (fromIntegral . fst) $ filter (\(id, properties) -> 
				anyOf traverse (\prop -> 
					prop^._1 =="type" && prop^._2 == "Wall"
				) properties
			) (ts^.T.tsTileProperties)

instance A.FromJSON (State World ()) where
	parseJSON (A.Object v) = do
		tileSets <- v A..: "tileset"
		foldM loadTileSet (return ()) tileSets

		where
			loadTileSet worldState (A.Object v) = do
				tsName <- v A..: "name"
				tsFilename <- v A..: "filename"
				tsBaseDir <- v A..: "base_directory"
				tsImageWidth <- v A..: "imageWidth"
				tsImageHeight <- v A..: "imageHeight"
				tsTiles <- v A..: "data"

				let Right filename = (FP.toText $ tsBaseDir FP.</> tsFilename)
				let newWorldState = do
					wComplexTileset (tsName) .= (Just $ newComplexTileset $ 
						newImage
							& iSource .~ (T.unpack filename)
							& iWidth .~ tsImageWidth
							& iHeight .~ tsImageHeight
						)

				newWorldState' <- foldM (loadTiles tsName) newWorldState tsTiles
				return (worldState >> newWorldState')

			loadTiles name worldState (A.Object v) = do
				width <- v A..: "width"
				height <- v A..: "height"			
				posX <- v A..: "offset_x"
				posY <- v A..: "offset_y"
				tileName <- v A..: "tileId"

				return $ traceShow tileName $ worldState >> do
					wAddComplexTile name tileName (posX, posY) (width, height)


type LoadTileset = B.ByteString
load :: IO B.ByteString
load = do
	B.readFile "tileset_compiled.json" 

loadComplexTilesets :: B.ByteString -> State World ()
loadComplexTilesets input = do
	let Right updateWorld = A.eitherDecode input :: (Either String (State World ()))
	updateWorld

