module World where

import Test.HUnit
import Game.Render.World
import Control.Lens

--tests :: Test
--tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

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
