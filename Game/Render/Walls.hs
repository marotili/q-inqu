{-# LANGUAGE TemplateHaskell #-}
module Game.Render.Walls where

import Debug.Trace
import qualified Data.Map as Map
import Control.Lens
import System.Random
import Game.Render.World
import qualified Game.Render.World as R
import Control.Monad.State.Strict

data WallTile = WallTile
	{ _wtPrefabName :: String
	, _wtPossibleRightTiles :: [(String, Int, Int)] -- x, y offsets
	, _wtPossibleBottomTiles :: [(String, Int, Int)]
	}

data WallTileManager = WallTileManager
	{ _wtmWallTiles :: Map.Map String WallTile
	}

makeLenses ''WallTile
makeLenses ''WallTileManager
wallBaseData = 
	[("Wall2", 90, 0), ("Wall2_2", 90, 0), ("Wall4", 90, 0), ("Wall4_2", 90, 0)
	, ("Wall5", 90, 0), ("Wall5_2", 90, 0), ("Wall6", 80, 0), ("Wall6_2", 80, 0)]

wallBaseData2 =
	[("Wall1", 0, 60), ("Wall1_2", 0, 60), ("Wall3", 0, 60), ("Wall3_2", 0, 60)
	, ("Wall4", 0, 40), ("Wall4_2", 0, 60)
	]

wallTileManager = WallTileManager Map.empty
	& wtmWallTiles . at "Wall1" .~ (Just $ newWallTile "Wall1" 
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall1_2" .~ (Just $ newWallTile "Wall1_2" 
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall2" .~ (Just $ newWallTile "Wall2"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall2_2" .~ (Just $ newWallTile "Wall2_2"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall3" .~ (Just $ newWallTile "Wall3"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall3_2" .~ (Just $ newWallTile "Wall3_2"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall4" .~ (Just $ newWallTile "Wall4"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall4_2" .~ (Just $ newWallTile "Wall4_2"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall5" .~ (Just $ newWallTile "Wall5"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall5_2" .~ (Just $ newWallTile "Wall5_2"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall6" .~ (Just $ newWallTile "Wall6"
		wallBaseData wallBaseData2
		)
	& wtmWallTiles . at "Wall6_2" .~ (Just $ newWallTile "Wall6_2"
		wallBaseData wallBaseData2
		)

newWallTile name right bottom = WallTile name right bottom

initWalls = do
	--let wr' = wUpdate (
	wAddComplexTile "background" "b1" (500, 500) (470, 470) (0, 0)
	objId <- R.wObjectFromPrefab "b1" ("b0")
	R.wLayerObject "BackgroundLayer" "b0" .= (Just $ R.newRenderObject objId (40, 30) 0)

	objId1 <- R.wObjectFromPrefab "Flower1" "flower"
	objId2 <- R.wObjectFromPrefab "Plant3" "plant"
	objId3 <- R.wObjectFromPrefab "Flower2" "flower2"
	R.wLayerObject "BottomLayer" "flower" .= (Just $ R.newRenderObject objId1 (250, 150) 0)
	R.wLayerObject "BottomLayer" "flower2" .= (Just $ R.newRenderObject objId2 (250, 200) 0)
	R.wLayerObject "BottomLayer" "plant" .= (Just $ R.newRenderObject objId3(200, 150) 0)
	--return wr'

generateWalls :: Int -> Int -> StdGen -> State World ()
generateWalls width height stdGen = do
	let wtm = wallTileManager

	let (first, g') = selectOne (Map.keys $ wtm^.wtmWallTiles) stdGen
	objId <- R.wObjectFromPrefab first (show g')
	R.wLayerObject "BottomLayer" (show g') .= (Just $ R.newRenderObject objId (0, 0) 0)

	(ox', oy', g2, topRightName) <- selectRight 0 0 g' first
	(ox2', oy2', g3, bottomLeftName) <- selectBottom 0 0 g2 first
	(_, _, g4, _) <- selectRight 0 oy2' g3 bottomLeftName
	(_, _, g5, _) <- selectBottom ox' 0 g4 topRightName
	return ()

	where
		selectOne names gen = let (index, gen') = randomR (0, length names - 1) gen
			in (names !! index, gen')

		choicesRight name = let wtm = wallTileManager
			in (wtm^.wtmWallTiles.at name._Just.wtPossibleRightTiles)

		choicesBottom name = let wtm = wallTileManager
			in (wtm^.wtmWallTiles.at name._Just.wtPossibleBottomTiles)

		selectRight offsetX offsetY gen lastName
			| offsetX + ox > width = return (offsetX, offsetY, g', lastName)
			| offsetY + oy > height = return (offsetX, offsetY, g', lastName)
			| otherwise = do
				objId <- R.wObjectFromPrefab newName (show g')
				R.wLayerObject "BottomLayer" (show g') .= (Just $ R.newRenderObject objId (fromIntegral $ offsetX + ox, fromIntegral $ offsetY + oy) 0)
				(ox', oy', g'', lastName) <- selectRight (offsetX + ox) (offsetY + oy) g' newName
				return $ traceShow (ox', oy') (ox', oy', g'', lastName)

				where ((newName, ox, oy), g') = selectOne (choicesRight lastName) gen

		selectBottom offsetX offsetY gen lastName
			| offsetX + ox > width = return (offsetX, offsetY, g', lastName)
			| offsetY + oy > height = return (offsetX, offsetY, g', lastName)
			| otherwise = do
				objId <- R.wObjectFromPrefab newName (show g')
				R.wLayerObject "BottomLayer" (show g') .= (Just $ R.newRenderObject objId (fromIntegral $ offsetX + ox, fromIntegral $ offsetY + oy) 0)
				(ox', oy', g'', lastName) <- selectBottom (offsetX + ox) (offsetY + oy) g' newName
				return $ traceShow (ox', oy') (ox', oy', g'', lastName)

				where ((newName, ox, oy), g') = selectOne (choicesBottom lastName) gen
		