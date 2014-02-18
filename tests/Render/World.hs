module World where

import Test.HUnit
import Game.Render.World
import Control.Lens

import Game.World.Import.Tiled

--tests :: Test
--tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

testLoadMap :: Test
testLoadMap = TestCase (do
		tm <- tMap
		let world = loadMapFromTiled tm

		let nWorld = wUpdate (do
				Just tsId <- use $ mapHashes.gameTilesets.at "sprite_klein3"
				wObject "Player1" .= (Just $ newObject tsId 0)
				Just objId <- use $ mapHashes.gameObjects.at "Player1"
				wLayer "ObjectLayer" .= (Just $ newLayer ObjectLayerType)
				wLayerObject "ObjectLayer" "Player1" .= (Just $ newRenderObject objId (50, 50))

			) world

		print nWorld
		return ()
	)

testAddRemove :: Test
testAddRemove = TestCase (do
		let w = newWorld
			& wSize .~ (50, 50)
			& wTileSize .~ (24, 24)
			& wTileset "BaseTileset" .~ Just newTileset
				& wTileSize .~ (24, 24)
		let wb = w 
			& wTileset "TestTs" .~ Just newTileset
		let w' = w 
			& wTileset "TestTileset" .~ Just newTileset
			& wTileset "TestTs" .~ Just newTileset
		let w'' = w'
			& wTileset "TestTileset" .~ Nothing
		assertBool "world differs after add and remove" (shallowEqual wb w'')
	)
