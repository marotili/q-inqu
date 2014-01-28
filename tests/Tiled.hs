module Main where
	
import Data.Tiled

import qualified Data.Map as Map
import Control.Monad

main :: IO ()
main = do
	--tiledMap <- loadMapFile "data/desert.tmx"
	--let tileSet = head . mapTilesets $ tiledMap
	--mapM_ (print . pixelCoordinates tileSet) $ concat [map ((\x -> x - 1) . fromIntegral . tileGid . snd) (Map.toList (layerData layer))  | layer <- mapLayers tiledMap]


	return ()

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
--		x = tileId `mod` numX
--		y = tileId `div` numX

