{-# LANGUAGE NamedFieldPuns, Rank2Types #-}
module Game.World.Import.Tiled 
	( 
	  mapBoulders, mapWallTiles
	, mapWallPositions

	, queryObject, objectPos, objectsByName
	) where

import Control.Lens
import Data.Tiled
import Data.Maybe
import qualified Data.Map as Map
import Data.Word

tMap = loadMapFile "data/sewers.tmx"

-- Helpers
-- |Access the positions of an object. Converts float to int.
objectPos :: Lens' Object (Float, Float)
objectPos = lens
	(\obj -> (fromIntegral $ obj^.objectX, fromIntegral $ obj^.objectY))
	(\obj (x, y) -> obj & objectX .~ round x & objectY .~ round y)

mapSize :: Getter TiledMap (Float, Float)
mapSize = to 
	(\tm -> (fromIntegral $ tm^.mapWidth, fromIntegral $ tm^.mapHeight))
	--(\tm (w, h) -> tm & mapWidth .~ round w & mapHeight .~ round h)

numTiles :: Getter Tileset Int
numTiles = to _numTiles
	where
		_numTiles :: Tileset -> Int
		_numTiles ts = fromJust $ do
				x <- numX
				y <- numY
				return $ x * y
			where
				numX = ts^.tsImages^?_head.iWidth >>= \w -> return (w `div` ts^.tsTileWidth)
				numY = ts^.tsImages^?_head.iHeight >>= \h -> return (h `div` ts^.tsTileHeight)

objectGid' :: Getter Object (Maybe Int)
objectGid' = to (\o -> fmap fromIntegral (o^.objectGid))

tileGid' :: Getter Tile Int
tileGid' = to (\o -> fromIntegral $ o^.tileGid)

tsInitialGid' :: Getter Tileset Int
tsInitialGid' = to (\ts -> fromIntegral (ts^.tsInitialGid))

tilesetOfTile :: TiledMap -> Int -> Tileset
tilesetOfTile tm gid = fromJust $ findOf
	traverse
	(\ts -> ts^.tsInitialGid' <= gid && ts^.tsInitialGid' + ts^.numTiles > gid)
	(tm^.mapTilesets)

objectsByName :: String -> Traversal' Object Object
objectsByName name = filtered (\obj -> obj^.objectName._Just == name)

queryObject :: TiledMap -> String -> Maybe Object
queryObject tm name = findOf 
	traverse 
	(\obj -> obj^.objectName._Just == name) 
	(tm^.mapLayers.traverse._ObjectLayer.layerObjects)

mapTileByType :: TiledMap -> String -> [Int]
mapTileByType tm typeName = map addGid $ filter cond (
		foldr (\ts l -> takeTilesetWithProps ts ++ l) [] (tm^.mapTilesets)
	)
	where
		cond (_, _, properties) = anyOf traverse (\prop -> 
				prop^._1 =="type" && prop^._2 == typeName
			) properties
		
		addGid :: (Tileset, Word32, [(String, String)]) -> Int
		addGid (ts, tileGid, properties) = fromIntegral (ts^.tsInitialGid) + fromIntegral tileGid

takeTilesetWithProps :: Tileset -> [(Tileset, Word32, [(String, String)])]
takeTilesetWithProps ts = [(ts, tileId, props) | (tileId, props) <- ts^.tsTileProperties]

-- | map -> list of tile ids -> (position, tile)
mapTiles :: TiledMap -> [Int] -> [((Int, Int), Tile)]
mapTiles tm tileGids = 
		filter cond (Map.toList (tm^.mapLayers.traverse._Layer.layerData))
	where
		cond (pos, tile) = tile^.tileGid' `elem` tileGids
		--cond _ = True

mapIdxToCoords :: TiledMap -> (Int, Int) -> (Float, Float)
mapIdxToCoords tm (x, y) = (fromIntegral $ tm^.mapWidth * x, fromIntegral $ tm^.mapHeight * y)


tileIs :: TiledMap -> Int -> String -> Bool
tileIs tm gid name = gid `elem` mapTileByType tm name
-- Data
mapWallPositions :: TiledMap -> [(Float, Float)]
mapWallPositions tm = map (mapIdxToCoords tm . fst) $ mapTiles tm (mapWallTiles tm)

--mapBoulderPositions :: TiledMap -> [(Float, Float)]
--mapBoulderPositions tm = map (mapIdxToCoords tm . fst) $ mapTiles tm (mapBoulders tm)

mapWallTiles :: TiledMap -> [Int]
mapWallTiles tm = mapTileByType tm "Wall"

mapBoulders :: TiledMap -> [Object]
mapBoulders tm = tm^..mapLayers.traverse._ObjectLayer.layerObjects.traverse.filtered (\obj ->
	    case obj^.objectGid of
	        Just gid -> tileIs tm (fromIntegral gid) "Boulder"
	        Nothing -> False
    )

