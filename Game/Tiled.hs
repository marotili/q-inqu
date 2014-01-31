module Game.Tiled where
	
import qualified Data.Tiled as T

import qualified Data.Map as Map
import Control.Monad

import Game.Map
import qualified Game.Render.Map as RM

mapConfigFromTiled :: T.TiledMap -> MapConfig
mapConfigFromTiled tiledMap = MapConfig
	{ mapWidth = T.mapWidth tiledMap
	, mapHeight = T.mapHeight tiledMap
	, mapNeighborhoodFunc = clipNeighborhood
	, mapCellTiles = tiles
	}
	where
		tiles = foldr (\(k, v) -> Map.insert k (fromIntegral $ T.tileGid v)) Map.empty $ 
			concatMap (Map.toList . T.layerData) (T.mapLayers tiledMap)

renderMapFromTiled :: T.TiledMap -> RM.Map
renderMapFromTiled tm = renderMap
	where
		renderMap = RM.newRenderMap gameMap 
			( fromIntegral $ T.mapTileWidth tm
			, fromIntegral $ T.mapTileHeight tm)
		gameMap = mapNew (mapConfigFromTiled tm)

--main :: IO ()
--main = do
--	--tiledMap <- loadMapFile "data/desert.tmx"
	--let tileSet = head . mapTilesets $ tiledMap
	--mapM_ (print . pixelCoordinates tileSet) $ 
	--concat [map ((\x -> x - 1) . fromIntegral . tileGid . snd)
	-- --(Map.toList (layerData layer))  | layer <- mapLayers tiledMap]


	--return ()

--pixelCoordinates :: Tileset -> Int -> (Int, Int)
--pixelCoordinates tileSet tileGid = (
--			x * tsTileWidth tileSet + spacing*(x-1) + margin,
--		 	y * tsTileHeight tileSet + spacing*(y-1) + margin)
--	where
--		tileId = tileGid - tsInitialGid tileSet
--		margin = tsMargin tileSet
--		spacing = tsSpacing tileSet
--		image = head . tsImages $ tileSet
--		numX = iWidth image `div` tsTileWidth tileSet
--		x = tileId `mod` numX + 1
--		y = tileId `div` numX + 1

