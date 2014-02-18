{-# LANGUAGE TemplateHaskell, Rank2Types, NamedFieldPuns #-}
module Game.Render.World where

import Data.List
import qualified Data.Tiled as T
import qualified Game.World.Import.Tiled as T
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

import Control.Lens

-- | 

type TilesetId = Int
type LayerId = Int

type TileId = Int
type LocalTileId = TileId
type TileIdOffset = Int

type ObjectId = Int

type Position = (Float, Float)

data World = World
	{ _mapWidth, _mapHeight :: Int
	, _mapTileWidth, _mapTileHeight :: Int
	, _mapTilesets :: Map.Map TilesetId Tileset
	, _mapTsOffsets :: Map.Map TilesetId TileIdOffset
	, _mapLayers :: Map.Map LayerId MapLayer
	, _mapObjects :: Map.Map ObjectId Object 
	, _mapHashes :: ObjHashes
	--, _mapUpdates :: MapUpdates
	, _mapNextId :: Int

	, _mapUpdateLayers :: Set.Set LayerId
	} deriving (Eq, Show)

--data MapUpdates = MapUpdates
--	{ 
--	} deriving (Eq, Show)

data ObjHashes = ObjHashes
	{ _gameObjects :: Map.Map String ObjectId
	, _hashLayers :: Map.Map String LayerId
	, _gameTilesets :: Map.Map String TilesetId
	} deriving (Eq, Show)

data TileType = 
	  TileTypeDefault
	| TileTypeWall
	deriving (Eq, Show)

data Image = Image
	{ _iSource :: FilePath
	, _iWidth, _iHeight :: Int
	} deriving (Eq, Show)


data Tileset = Tileset
	{ _tsTileWidth, _tsTileHeight :: Int
	, _tsSpacing, _tsMargin :: Int
	, _tsImage :: Image
	, _tsTileTypes :: Map.Map LocalTileId TileType
	} deriving (Eq, Show)

data Tile = Tile
	{ _tileId :: TileId
	} deriving (Eq, Show)

data Object = Object
	{ _objTsId :: TilesetId
	, _objLocalId :: LocalTileId
	} deriving (Eq, Show)

newObject ts lid = Object
	{ _objTsId = ts
	, _objLocalId = lid
	}

data RenderObject = RenderObject
	{ _roId :: ObjectId
	, _roPos :: Position
	} deriving (Eq, Show)

newRenderObject oId pos = RenderObject
	{ _roId = oId
	, _roPos = pos
	}
data LayerType = TileLayerType | ObjectLayerType
	deriving (Eq, Show)

data MapLayer = TileLayer
	{ _layerTiles :: Map.Map (Int, Int) TileId
	}
	| ObjectLayer
	{ _layerObjects :: Map.Map ObjectId RenderObject
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

newWorld :: World
newWorld = World
	{ _mapWidth = 0
	, _mapHeight = 0
	, _mapTileWidth = 0
	, _mapTileHeight = 0
	, _mapTilesets = Map.empty
	, _mapTsOffsets = Map.empty
	, _mapLayers = Map.empty
	, _mapObjects = Map.empty
	, _mapHashes = newHashes
	, _mapNextId = 1
	, _mapUpdateLayers = Set.empty
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

wLayerObject :: String -> String -> Lens' World (Maybe RenderObject)
wLayerObject layerName objName = lens get set
	where
		get world = let Just oId = world^.mapHashes.gameObjects.at objName
			in world^?!wLayer layerName._Just._layerObject oId

		set world mro = let 
				Just oId = world^.mapHashes.gameObjects.at objName
				Just lId = world^.mapHashes.hashLayers.at layerName
			in world 
				& wLayer layerName._Just._layerObject oId .~ mro
				& mapUpdateLayers %~ Set.insert lId

_layerObject :: ObjectId -> Lens' MapLayer (Maybe RenderObject)
_layerObject oId = lens get set
	where 
		get ml = case ml of
			ObjectLayer {_layerObjects } -> _layerObjects^.at oId
			otherwise -> Nothing
		set ml mro = case ml of
			ol@ObjectLayer { _layerObjects } -> ol & layerObjects . at oId .~ mro
			otherwise -> ml 

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
	}

newImage :: Image
newImage = Image
	{ _iSource = ""
	, _iWidth = 0
	, _iHeight = 0
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
wObjTileId obj = to (\world -> (world^?!mapTsOffsets.at (obj^.objTsId)._Just) + (obj^.objLocalId))

wLayerNumObjects :: String -> Getter World Int
wLayerNumObjects layerName = to get
	where get world = case layer of
				TileLayer { _layerTiles } -> Map.size _layerTiles
				ObjectLayer { _layerObjects } -> Map.size _layerObjects
			where
				Just lId = world^.mapHashes.hashLayers.at layerName
				Just layer = world^.mapLayers.at lId

wTileIds :: String -> Getter World [TileId]
wTileIds layerName = to get
	where
		get world = case layer of
				TileLayer { _layerTiles } -> Map.elems _layerTiles
				ObjectLayer { _layerObjects } -> map getObjectTileId $ Map.elems _layerObjects
			where
				Just lId = world^.mapHashes.hashLayers.at layerName
				Just layer = world^.mapLayers.at lId

				getObjectTileId ro = 
					let obj = (world^?!mapObjects.at (ro^.roId)._Just)
					in (world^.wObjTileId obj)


wTilePos :: String -> Getter World [(Float, Float)]
wTilePos layerName = to get
	where
		get world = case layer of
			TileLayer { _layerTiles } -> map (\(x, y) ->
					( fromIntegral $ x * world^.mapTileWidth
					, fromIntegral $ y * world^.mapTileHeight)
				) $ Map.keys _layerTiles
			ObjectLayer { _layerObjects } -> sortBy sortY $ map (^.roPos) $ Map.elems _layerObjects
			where
				Just lId = world^.mapHashes.hashLayers.at layerName
				Just layer = world^.mapLayers.at lId

				sortY :: (Float, Float) -> (Float, Float) -> Ordering
				sortY (x, y) (x2, y2) 
					| y <= y2 = LT
					| otherwise = GT

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

