{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Game.Render.World where

import qualified Data.Tiled as T
import qualified Game.World.Import.Tiled as T
import Control.Monad.State
import qualified Data.Map as Map
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
	-- | The position changes often
	, _mapObjects :: Map.Map ObjectId Object 
	, _mapHashes :: ObjHashes
	, _mapNextId :: Int
	} deriving (Eq, Show)

data ObjHashes = ObjHashes
	{ _gameObjects :: Map.Map String ObjectId
	, _hashLayers :: Map.Map String LayerId
	, _gameTilesets :: Map.Map String TilesetId
	} deriving (Eq, Show)

data TileType = 
	TypeWall
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
	{ _objPosition :: Position
	, _objTsId :: TilesetId
	, _objLocalId :: LocalTileId
	} deriving (Eq, Show)

data RenderObject = RenderObject
	{ _roId :: ObjectId
	, _roPos :: Position
	} deriving (Eq, Show)

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
	}

_wId :: State World Int
_wId = do
	id_ <- use mapNextId
	mapNextId += 1
	return id_

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
			Just ts -> wUpdate (wAddTileset name) world
			Nothing -> wUpdate (wRemoveTileset name) world

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

wLayer :: String -> Lens' World MapLayer
wLayer = lens get set
	where
		get world = 
			let mlayerId = world^.mapHashes.hashLayers.at name
			in mlayerId >>= \layerId -> world^.mapLayers . at layerId
		set world mlayer = case mlayer of
			Just layer -> wUpdate (wAddLayer name) world
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
		numTiles ts | tileWidth == 0 = 0
					| tileHeight == 0 = 0
					| otherwise = (width `div` tileWidth) * (height `div` tileHeight)
			where
				width = ts^.tsImage.iWidth - ts^.tsMargin + ts^.tsSpacing
				height = ts^.tsImage.iHeight - ts^.tsMargin
				tileWidth = ts^.tsTileWidth + ts^.tsSpacing
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

--mlTileIds :: MapLayer -> [TileId]
--mlCoords :: MapLayer -> [(Float, Float)]

shallowEqual :: World -> World -> Bool
shallowEqual w1 w2 = traceShow (w1^.mapTsOffsets, w2^.mapTsOffsets, w, h, tw, th, ts, tsOff, ml, obj, hashs) $ w && h && tw && th && ts && tsOff && ml && obj && hashs
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

 