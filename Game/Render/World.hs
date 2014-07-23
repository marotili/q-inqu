{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, TemplateHaskell, Rank2Types, NamedFieldPuns #-}
module Game.Render.World
(
) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

import qualified Filesystem.Path.CurrentOS as FP
import qualified Filesystem.Path as FP
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

import Control.Lens

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

instance A.FromJSON FP.FilePath where
  parseJSON (A.String v) = return $ FP.fromText v
  parseJSON _ = mzero

type TilesetId = Int

type ObjectName = String
type TilesetName = String
type TileName = String
type TilePos = (Int, Int)
type TileSize = (Int, Int)
type Position = (Float, Float)
type Origin = (Float, Float)


data RenderObject = RenderObject
  { _roTilesetName :: !TilesetName
  , _roLocalId :: !LocalTileId
  , _roPos :: !Position
  , _roRotation :: !Float
  , _roOrigin :: !Position
  }

newRenderObject = RenderObject 
  { _roTilesetName = ""
  , _roLocalId = -1
  , _roPos = (0, 0)
  , _roRotation = 0
  , _roOrigin = (0, 0)
  }

data Image = Image
	{ _iSource :: !FilePath
	, _iWidth, _iHeight :: !Int
	} deriving (Eq, Show)

newImage = Image
  { _iSource = ""
  , _iWidth = 0
  , _iHeight = 0
  }

type LocalTileId = Int

data Tileset = Tileset
	{ _ctsImage :: !Image
	, _ctsTiles :: !(Map.Map LocalTileId CtsData)
	, _ctsNextLocalId :: LocalTileId
	} deriving (Eq, Show)

newTileset :: Image -> Tileset
newTileset image = Tileset 
  { _ctsImage = image
  , _ctsTiles = Map.empty
  , _ctsNextLocalId = 0
  }

data CtsData = CtsData
	{ _ctsdPos :: !(Int, Int)
	, _ctsdSize :: !(Int, Int)
	} deriving (Eq, Show)

data MapLayer =
  -- | static data (render objects dont change)
  StaticLayer
  { _staticLayerObjects :: !([RenderObject])
  }
  -- | dynamic data, needs fast access to render object
  | DynamicLayer
  { _dynamicLayerObjects :: !(Map.Map ObjectName RenderObject)
  }

_StaticLayer :: Prism' MapLayer MapLayer
_StaticLayer = prism' id (\l -> case l of StaticLayer {} -> Just l; _ -> Nothing)

_DynamicLayer :: Prism' MapLayer MapLayer
_DynamicLayer = prism' id (\l -> case l of DynamicLayer {} -> Just l; _ -> Nothing)

data ObjectPrefab = ObjectPrefab
  { _opTilesetName :: TilesetName
  , _opTileId :: LocalTileId
  , _opOrigin :: Origin
  }

type LayerName = String

data RenderWorld = RenderWorld
  { _mapTilesets :: !(Map.Map TilesetName Tileset)
  , _mapLayers :: !(Map.Map LayerName MapLayer)
  , _mapPrefabs :: !(Map.Map TileName ObjectPrefab)
  , _mapObjects :: !(Map.Map ObjectName LayerName)
  }

emptyRenderWorld = RenderWorld
  { _mapTilesets = Map.empty
  , _mapLayers = Map.empty
  , _mapPrefabs = Map.empty
  , _mapObjects = Map.empty
  }

makeLenses ''Tileset
makeLenses ''Image
makeLenses ''RenderObject
makeLenses ''CtsData
makeLenses ''MapLayer
makeLenses ''ObjectPrefab
makeLenses ''RenderWorld

rwUpdate :: State RenderWorld () -> RenderWorld -> RenderWorld
rwUpdate s w = execState s w

rwTileset :: TilesetName -> Lens' RenderWorld (Maybe Tileset)
rwTileset name = lens get set
  where
    get renderWorld =
      renderWorld^.mapTilesets . at name
    set renderWorld mTileset = case mTileset of
      Just ts -> rwUpdate (do
        mapTilesets.at name .= Just ts
        ) renderWorld
      Nothing -> renderWorld

rwAddTile :: TilesetName -> TileName -> TilePos -> TileSize -> Origin -> State RenderWorld ()
rwAddTile tilesetName tileName tilePos tileSize origin = do
  mTileset <- use $ mapTilesets . at tilesetName
  case mTileset of
    Just tileset -> do
      let nextId = tileset^.ctsNextLocalId
      mapTilesets.at tilesetName._Just.ctsNextLocalId += 1
      mapTilesets.at tilesetName._Just.ctsTiles.at nextId .= (Just $ CtsData tilePos tileSize)
      mapPrefabs.at tileName .= (Just $ ObjectPrefab
          { _opTilesetName = tilesetName
          , _opTileId = nextId
          , _opOrigin = origin
          }
        )
    Nothing -> error "No tileset found"

rwAddObject :: ObjectName -> State RenderWorld LayerName
rwAddObject objectName = do
  mapLayers.at "Default"._Just .= (DynamicLayer Map.empty)
  mapLayers.at "Default"._Just.dynamicLayerObjects.at objectName .= (Just $
      newRenderObject
    )
  mapObjects.at objectName .= (Just $ "Default")
  return "Default"

rwAddStaticObject :: TileName -> Position -> Float -> State RenderWorld ()
rwAddStaticObject tileName position rotation = do
  return ()

rwUpdateRenderObjectTile :: ObjectName -> TileName -> State RenderWorld ()
rwUpdateRenderObjectTile objectName tileName = do
  mLayerName <- use $ mapObjects.at objectName :: State RenderWorld (Maybe LayerName)
  layerName <- case mLayerName of
    Nothing -> rwAddObject objectName
    Just name -> return name

  mPrefab <- use $ mapPrefabs.at tileName
  prefab <- case mPrefab of
    Nothing -> error "tile not found"
    Just prefab -> return prefab

  Just layer <- use $ mapLayers.at layerName :: State RenderWorld (Maybe MapLayer)
  --let mRenderObject = (_dynamicLayerObjects layer)^.at objectName :: Maybe RenderObject

  let mRenderObject = Nothing -- layer^?_DynamicLayer.dynamicLayerObjects.at objectName :: Maybe RenderObject

  case mRenderObject of 
    Just renderObject ->
      error "not implemented"
      --mapLayers.at layerName._Just._DynamicLayer.dynamicLayerObjects.at objectName .= (Just (renderObject 
      --    & roTilesetName .~ (prefab^.opTilesetName)
      --    & roLocalId .~ (prefab^.opTileId)
      --    & roOrigin .~ (prefab^.opOrigin)
      --    )
      --    )
    Nothing -> error "unexpected"

rwUpdateRenderObjectInstance :: ObjectName -> Position -> Float -> State RenderWorld ()
rwUpdateRenderObjectInstance objectName pos rotation = do
  return ()

instance A.FromJSON (State RenderWorld ()) where
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

        let newWorldState = rwTileset (tsName) .= (Just $ newTileset $
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
        originX <- v A..: "origin_x" :: A.Parser Int
        originY <- v A..: "origin_y" :: A.Parser Int
        tileName <- v A..: "tileId"

        return $ worldState >> do
            rwAddTile name tileName (posX, posY) (width, height) (fromIntegral originX, fromIntegral originY)

type LoadTileset = B.ByteString
load :: IO B.ByteString
load = do
  B.readFile "tileset_compiled.json"

loadComplexTilesets :: B.ByteString -> State RenderWorld ()
loadComplexTilesets input = do
  let Right updateWorld = A.eitherDecode input :: (Either String (State RenderWorld ()))
  updateWorld
