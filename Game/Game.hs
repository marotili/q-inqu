{-# LANGUAGE TemplateHaskell, Arrows, BangPatterns #-}
module Game.Game where

import Control.Lens
import Debug.Trace
import qualified Game.Render.World as R

import qualified Game.World.Common as G
import qualified Game.World.Objects as G
import qualified Game.World.Delta as G
import qualified Game.World as G
import qualified Game.Render.Update as U

import qualified Game.World.Gen as Gen
import qualified Game.World.Gen.Terrain as Gen

import qualified Game.World.Import.Tiled as T
import qualified Data.Tiled as T
import Game.World.Wires
import Game.World.Lens
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Wire as W
import qualified Control.Wire.Unsafe.Event as W

import Control.Monad.State.Strict
import Control.Monad.RWS.Strict

import Game.World.Common

import Control.Arrow

data Game = Game
	{ _gameName :: !String
	, _gameTiled :: !T.TiledMap
	, _gamePlayerStartPos :: !(Float, Float)
	, _gameLogicWorld :: !G.World
	, _gameRenderWorld :: !R.World
	, _gameLastDelta :: !G.WorldDelta
	, _gameWorldManager :: !G.WorldManager
	, _gameRenderObjects :: ![U.Renderable]
	, _gameWire :: !(G.WorldWire () ())
	, _gameGenMap :: !Gen.GenMap
	}

newRenderConfig :: R.RenderConfig
newRenderConfig = execState (do
		-- players
		R.rcTiles . at "Player1S1" .= Just ("sprite_klein3", 0)
		R.rcTiles . at "Player1S2" .= Just ("sprite_klein3", 1)
		R.rcTiles . at "Player1S3" .= Just ("sprite_klein3", 2)
		R.rcTiles . at "Player1S4" .= Just ("sprite_klein3", 3)

		R.rcTiles . at "Player1N1" .= Just ("sprite_klein3", 8)
		R.rcTiles . at "Player1N2" .= Just ("sprite_klein3", 9)
		R.rcTiles . at "Player1N3" .= Just ("sprite_klein3", 10)
		R.rcTiles . at "Player1N4" .= Just ("sprite_klein3", 11)

		R.rcTiles . at "Player1W1" .= Just ("sprite_klein3", 16)
		R.rcTiles . at "Player1W2" .= Just ("sprite_klein3", 17)
		R.rcTiles . at "Player1W3" .= Just ("sprite_klein3", 18)
		R.rcTiles . at "Player1W4" .= Just ("sprite_klein3", 19)

		R.rcTiles . at "Player1E1" .= Just ("sprite_klein3", 28)
		R.rcTiles . at "Player1E2" .= Just ("sprite_klein3", 29)
		R.rcTiles . at "Player1E3" .= Just ("sprite_klein3", 30)
		R.rcTiles . at "Player1E4" .= Just ("sprite_klein3", 31)

		R.rcTiles . at "Player2S1" .= Just ("ghost_sprite", 0)
		R.rcTiles . at "Player2S2" .= Just ("ghost_sprite", 1)
		R.rcTiles . at "Player2S3" .= Just ("ghost_sprite", 2)
		R.rcTiles . at "Player2S4" .= Just ("ghost_sprite", 3)

		R.rcTiles . at "Player2N1" .= Just ("ghost_sprite", 4)
		R.rcTiles . at "Player2N2" .= Just ("ghost_sprite", 5)
		R.rcTiles . at "Player2N3" .= Just ("ghost_sprite", 6)
		R.rcTiles . at "Player2N4" .= Just ("ghost_sprite", 7)

		R.rcTiles . at "Player2W1" .= Just ("ghost_sprite", 8)
		R.rcTiles . at "Player2W2" .= Just ("ghost_sprite", 9)
		R.rcTiles . at "Player2W3" .= Just ("ghost_sprite", 10)
		R.rcTiles . at "Player2W4" .= Just ("ghost_sprite", 11)

		R.rcTiles . at "Player2E1" .= Just ("ghost_sprite", 12)
		R.rcTiles . at "Player2E2" .= Just ("ghost_sprite", 13)
		R.rcTiles . at "Player2E3" .= Just ("ghost_sprite", 14)
		R.rcTiles . at "Player2E4" .= Just ("ghost_sprite", 15)

		R.rcTiles . at "Player2E4" .= Just ("ghost_sprite", 15)
		R.rcTiles . at "Player2E4" .= Just ("ghost_sprite", 15)

		R.rcTiles . at "Player3S1" .= Just ("dino", 0)
		R.rcTiles . at "Player3S2" .= Just ("dino", 1)
		R.rcTiles . at "Player3S3" .= Just ("dino", 2)
		R.rcTiles . at "Player3S4" .= Just ("dino", 3)

		R.rcTiles . at "Player4S1" .= Just ("dino", 4)
		R.rcTiles . at "Player4S2" .= Just ("dino", 5)
		R.rcTiles . at "Player4S3" .= Just ("dino", 6)
		R.rcTiles . at "Player4S4" .= Just ("dino", 7)

		-- arrows
		R.rcTiles . at "ArrowW" .= Just ("arrow", 0)
		R.rcTiles . at "ArrowNW" .= Just ("arrow", 1)
		R.rcTiles . at "ArrowN" .= Just ("arrow", 2)
		R.rcTiles . at "ArrowNE" .= Just ("arrow", 3)
		R.rcTiles . at "ArrowE" .= Just ("arrow", 4)
		R.rcTiles . at "ArrowSE" .= Just ("arrow", 5)
		R.rcTiles . at "ArrowS" .= Just ("arrow", 6)
		R.rcTiles . at "ArrowSW" .= Just ("arrow", 7)

		R.rcTiles . at "WallW2" .= Just ("sewer_tileset", 8)
		R.rcTiles . at "WallS2" .= Just ("sewer_tileset", 9)
		R.rcTiles . at "WallE2" .= Just ("sewer_tileset", 10)

		-- duplicates
		R.rcTiles . at "WallSW2" .= Just ("sewer_tileset", 8)
		R.rcTiles . at "WallSE2" .= Just ("sewer_tileset", 10)

		R.rcTiles . at "WallSW1" .= Just ("sewer_tileset", 16)
		R.rcTiles . at "WallS1" .= Just ("sewer_tileset", 17)
		R.rcTiles . at "WallSE1" .= Just ("sewer_tileset", 18)

		R.rcTiles . at "WallN3" .= Just ("sewer_tileset", 4)
		R.rcTiles . at "WallNE3" .= Just ("sewer_tileset", 5)
		R.rcTiles . at "WallE3" .= Just ("sewer_tileset", 13)
		R.rcTiles . at "WallSE3" .= Just ("sewer_tileset", 21)
		R.rcTiles . at "WallS3" .= Just ("sewer_tileset", 1)
		R.rcTiles . at "WallSW3" .= Just ("sewer_tileset", 19)
		R.rcTiles . at "WallW3" .= Just ("sewer_tileset", 11)
		R.rcTiles . at "WallNW3" .= Just ("sewer_tileset", 3)
		R.rcTiles . at "WallCenter3" .= Just ("sewer_tileset", 12)

		R.rcTiles . at "WallOuterNW3" .= Just ("sewer_tileset", 6)
		R.rcTiles . at "WallOuterNE3" .= Just ("sewer_tileset", 7)
		R.rcTiles . at "WallOuterSW3" .= Just ("sewer_tileset", 22)
		R.rcTiles . at "WallOuterSE3" .= Just ("sewer_tileset", 23)

		R.rcTiles . at "FinalFloor" .= Just ("sewer_tileset", 33)
		R.rcTiles . at "NoMatch" .= Just ("sewer_tileset", 40)
		R.rcTiles . at "Wall" .= Just ("sewer_tileset", 41)

		--R.rcTiles . at "ItemBolt" .= Just ("items", 2)
	) $ R.emptyConfig

makeLenses ''Game

newGame :: String -> IO Game
newGame name = do
	tiledMap <- T.tMap
	let genMap = Gen.mkGenWorld
	(oldWorld, newWorld, delta, manager) <- mkGameWorld tiledMap (50, -200) genMap
	--print delta 

	complexTileset <- R.load 

	let renderWorld = mkRenderWorld tiledMap delta genMap complexTileset
	let (newRenderWorld, newRenderables) = 
		updateRender delta oldWorld newWorld renderWorld []

	let game = Game
		{ _gameName = name
		, _gameTiled = tiledMap
		, _gamePlayerStartPos = (50, -200)
		, _gameLogicWorld = newWorld
		, _gameLastDelta = delta
		, _gameWorldManager = manager
		, _gameRenderWorld = newRenderWorld
		, _gameRenderObjects = newRenderables
		, _gameWire = G.testwire
		, _gameGenMap = genMap
		}
	return game

gameStepWorld :: 
	   WorldWire () b 
	-> World 
	-> WorldManager 
	-> Rational 
	-> (WorldWire () b, (WorldManager, WorldDelta))
gameStepWorld w' world' state' dt' = newWire `seq` worldManager `seq` worldDelta `seq` (newWire, (worldManager, worldDelta))
	where
		dt = W.Timed (fromRational dt') ()
	-- run wires
		(newWire, worldManager, worldDelta) = runRWS (do
				(mx, newWire) <- W.stepWire w' dt (Right ())
				return newWire
			) world' state'

		--tracking = world'^.wCommon.wcTrack

--updateRender :: G.WorldDelta -> G.World -> G.World -> R.World -> (R.World, [U.Renderable])
updateRender delta oldWorld newWorld renderWorld renderablesIn = 
		let x = newRenderablesDeleted ++ newRenderables
		in x `seq` (renderWorld3, x)
	where
		-- remove objects from renderer
		(_, !renderWorld2', !removeRenderables) = runRWS (do
				U.removeRenderObjects
			) (oldWorld, delta, renderablesIn) renderWorld

		!newRenderablesDeleted = Set.toList $ Set.difference (Set.fromList renderablesIn) (Set.fromList removeRenderables)

		-- add objects to renderer
		(_, !renderWorld2, !newRenderables) = runRWS (do
				--updateTiled
				U.newRenderObjects
			) (newWorld, delta, newRenderablesDeleted) renderWorld2'

		-- update render objects
		(_, !renderWorld3, _) = runRWS U.update
			(newWorld, delta, newRenderablesDeleted ++ newRenderables) renderWorld2

updateOnlyGame :: Rational -> State Game ()
updateOnlyGame dt = do
	game <- get
	world <- use $ gameLogicWorld
	worldWire <- use $ gameWire
	oldManager <- use $ gameWorldManager

	let (!newWire, (!newManager, !newDelta)) = gameStepWorld worldWire world oldManager dt
	let !newWorld = G.applyDelta world newDelta

	gameLogicWorld .= newWorld
	gameLastDelta .= newDelta
	gameWorldManager .= newManager
	gameWire .= newWire

updateGame :: Rational -> State Game ()
updateGame dt = do
	game <- get
	renderablesIn <- use $ gameRenderObjects
	world <- use $ gameLogicWorld
	worldWire <- use $ gameWire
	oldManager <- use $ gameWorldManager
	renderWorld <- use $ gameRenderWorld

	let (!newWire, (!newManager, !newDelta)) = gameStepWorld worldWire world oldManager dt
	let !newWorld = G.applyDelta world newDelta
	let (!newRenderWorld, !newRenderables) = updateRender newDelta world newWorld renderWorld renderablesIn

	gameRenderObjects .= newRenderables
	gameRenderWorld .= newRenderWorld
	gameLogicWorld .= newWorld
	gameLastDelta .= newDelta
	gameWorldManager .= newManager
	gameWire .= newWire

	return ()

mkRenderWorld :: T.TiledMap -> G.WorldDelta -> Gen.GenMap -> R.LoadTileset -> R.World
mkRenderWorld tiledMap delta genMap complexTileset = nWorld
	where	
		renderWorld = R.loadMapFromTiled tiledMap
			& R.wRenderConfig .~ newRenderConfig

		nWorld = let w1 = 
				R.wUpdate (do
					R.loadComplexTilesets complexTileset
				) renderWorld
			in R.wUpdate (do

				--R.wComplexTileset "Monsters" .= (Just $ R.newComplexTileset)

				R.wLayer "BottomLayer" .= (Just $ R.newLayer R.TileLayerType)
				R.wLayer "ObjectLayer" .= (Just $ R.newLayer R.ObjectLayerType)
				R.wLayer "CObjectLayer" .= (Just $ R.newLayer R.ComplexLayerType)
				R.wLayer "TopLayer" .= (Just $ R.newLayer R.TileLayerType)

				--R.wObject "test" .= (Just $ R.newObject )
				Just objId <- use $ R.wObjectId "WolfFrontWalk1"
				R.wLayerObject "CObjectLayer" "WolfFrontWalk1" .= (Just $ R.newRenderObject (objId) (100, 100) 0)

				mapM_ (\((x, y), tileType) -> do
						tile <- use $ R.wTile (show tileType) -- TODO: change show to getter
						-- top tiles are transparent so we need a floor tile beneath them
						if (Gen.tileLayer genMap (x, y)) == "TopLayer"
							then  do
								floorTile <- use $ R.wTile "FinalFloor"
								R.wLayerTile "BottomLayer" (x, -y) .= (Just floorTile)
							else return ()
						R.wLayerTile (Gen.tileLayer genMap (x, y)) (x, -y) .= (Just tile)
					) (Map.toList $ genMap^.Gen.mapCompiledCells)

			) w1 -- renderWorld

--mkGameWorld :: Game -> IO (G.World, Delta, WorldManager)
mkGameWorld tiledMap startPos genMap = do
	let (worldManager, worldDelta) = execRWS (
			W.stepWire initWire (W.Timed 0 ()) (Right ())
		) world G.emptyWM

	let world' = G.applyDelta world worldDelta
	return (world, world', worldDelta, worldManager)


	where
		wallBoundaries = Gen.tileBoundaries genMap
		world = G.emptyW 
			{ _wTileBoundary = tiledMap^.T.mapTileSize
			}

		initWalls [] = proc input -> do
			returnA -< ()
		initWalls (((ox, oy), (px, py)):wallsData) = proc input -> do

			wallsId <- spawnObjectAt "Wall" (ox, -oy) -< input
			_ <- wLiftSetOnce setBoundary (
					[ (0, -py + oy)
					, (0, 0)
					, (px-ox, 0)
					, (px-ox, -py + oy)]
				) -< wallsId
			_ <- wLiftSetOnceVoid setStaticCollidable -< wallsId

			_ <- initWalls wallsData -< input
			returnA -< ()

		initWire = proc input -> do
			p1Id <- spawnObjectAt "Player1" startPos -< input
			p2Id <- spawnObjectAt "Player2" (200, -50) -< input
			p3Id <- spawnObjectAt "Player3" (200, -150) -< input
			p4Id <- spawnObjectAt "Player4" (200, -250) -< input

			_ <- animate (G.objectAnimation 1 G.South) -< p1Id
			_ <- animate (G.objectAnimation 2 G.South) -< p2Id
			_ <- animate (G.objectAnimation 3 G.South) -< p3Id
			_ <- animate (G.objectAnimation 4 G.South) -< p4Id

			_ <- wLiftSetOnce setBoundary G.playerBoundary -< p1Id
			_ <- wLiftSetOnce setBoundary G.playerBoundary -< p2Id
			_ <- wLiftSetOnce setBoundary G.playerBoundary -< p3Id
			_ <- wLiftSetOnce setBoundary G.playerBoundary -< p4Id

			_ <- initWalls wallBoundaries -< input

			itemId <- spawnObject "Bolt" -< input

			returnA -< ()

mkUIWorld :: Game -> R.World
mkUIWorld game = nWorld
	where	
		tiledMap = game^.gameTiled
		renderWorld = R.loadMapFromTiled tiledMap
		nWorld = R.wUpdate (do
				Just tsId <- use $ R.mapHashes.R.gameTilesets.at "heart"
				R.wObject "PlayerHealth" .= (Just $ R.newObject tsId 0)
				Just objId <- use $ R.mapHashes.R.gameObjects.at "PlayerHealth"
				R.wLayer "ObjectLayer" .= (Just $ R.newLayer R.ObjectLayerType)
				R.wLayerObject "ObjectLayer" "PlayerHealth" 
					.= (Just $ R.newRenderObject objId (10, -10) 0)
			) renderWorld
