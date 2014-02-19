{-# LANGUAGE TemplateHaskell, Arrows #-}
module Game.Game where

import Control.Lens
import qualified Game.Render.World as R

import qualified Game.World.Common as G
import qualified Game.World.Objects as G
import qualified Game.World.Delta as G
import qualified Game.World as G
import qualified Game.Render.Update as U

import qualified Game.World.Import.Tiled as T
import qualified Data.Tiled as T
import Game.World.Wires
import Game.World.Lens
import qualified Data.Set as Set
import qualified Control.Wire as W

import Control.Monad.State
import Control.Monad.RWS

import Game.World.Common

import Control.Arrow

data Game = Game
	{ _gameName :: String
	, _gameTiled :: T.TiledMap
	, _gamePlayerStartPos :: (Float, Float)
	, _gameLogicWorld :: G.World
	, _gameRenderWorld :: R.World
	, _gameLastDelta :: G.WorldDelta
	, _gameWorldManager :: G.WorldManager
	, _gameRenderObjects :: [U.Renderable]
	, _gameWire :: G.WorldWire () ()
	}

makeLenses ''Game

newGame :: String -> IO Game
newGame name = do
	tiledMap <- T.tMap
	(oldWorld, newWorld, delta, manager) <- mkGameWorld tiledMap (50, 50)
	let renderWorld = mkRenderWorld tiledMap delta
	let (newRenderWorld, newRenderables) = updateRender delta oldWorld newWorld renderWorld []
	let game = Game
		{ _gameName = name
		, _gameTiled = tiledMap
		, _gamePlayerStartPos = (50, 50)
		, _gameLogicWorld = newWorld
		, _gameLastDelta = delta
		, _gameWorldManager = manager
		, _gameRenderWorld = newRenderWorld
		, _gameRenderObjects = newRenderables
		, _gameWire = G.testwire
		}
	return game

gameStepWorld :: 
	   WorldWire () b 
	-> World 
	-> WorldManager 
	-> Rational 
	-> (WorldWire () b, (WorldManager, WorldDelta))
gameStepWorld w' world' state' dt' = (w, (worldManager, worldDelta))
	where
		dt = W.Timed (fromRational dt') ()
	-- run wires
		(w, worldManager, worldDelta) = runRWS (do
				(mx, newWire) <- W.stepWire w' dt (Right ())
				return newWire
			) world' state'

--updateGameServer :: Game -> IO Game
--updateGameServer game session' = do
--	(dt, session) <- stepSession session'

--	let state' = (game^.gameWorldManager)
--	let world' = (game^.gameLogicWorld)
--	let w' = (game^.gameWire)

--	-- run wires
--	((_, w), worldManager, worldDelta) <- runRWS (
--		stepWire w' dt (Right ())
--		) world' state'

--	let world' = applyDelta world delta

--	return $ game 
--		& gameWorldManager .~ worldManager
--		& gameWire .~ w
--		& gameLastDelta .~ worldDelta
--		& gameLogicWorld .~ world'

--updateRender :: G.WorldDelta -> G.World -> G.World -> R.World -> (R.World, [U.Renderable])
updateRender delta oldWorld newWorld renderWorld renderablesIn = (renderWorld3, newRenderablesDeleted ++ newRenderables)
	where
		-- remove objects from renderer
		(_, renderWorld2', removeRenderables) = runRWS (do
				U.removeRenderObjects
			) (oldWorld, delta, renderablesIn) renderWorld

		newRenderablesDeleted = Set.toList $ Set.difference (Set.fromList renderablesIn) (Set.fromList removeRenderables)

		-- add objects to renderer
		(_, renderWorld2, newRenderables) = runRWS (do
				--updateTiled
				U.newRenderObjects
			) (newWorld, delta, newRenderablesDeleted) renderWorld2'

		-- update render objects
		(_, renderWorld3, _) = runRWS U.update
			(newWorld, delta, newRenderablesDeleted ++ newRenderables) renderWorld2

updateGame :: Rational -> State Game ()
updateGame dt = do
	game <- get
	renderablesIn <- use $ gameRenderObjects
	world <- use $ gameLogicWorld
	worldWire <- use $ gameWire
	oldManager <- use $ gameWorldManager
	renderWorld <- use $ gameRenderWorld

	let (newWire, (newManager, newDelta)) = gameStepWorld worldWire world oldManager dt
	let newWorld = G.applyDelta world newDelta
	let (newRenderWorld, newRenderables) = updateRender newDelta world newWorld renderWorld renderablesIn

	gameRenderObjects .= newRenderables
	gameRenderWorld .= newRenderWorld
	gameLogicWorld .= newWorld
	gameLastDelta .= newDelta
	gameWorldManager .= newManager
	gameWire .= newWire

	return ()
mkRenderWorld :: T.TiledMap -> G.WorldDelta -> R.World
mkRenderWorld tiledMap delta = nWorld
	where	
		renderWorld = R.loadMapFromTiled tiledMap
		nWorld = R.wUpdate (do
				R.wLayer "ObjectLayer" .= (Just $ R.newLayer R.ObjectLayerType)
			) renderWorld

--mkGameWorld :: Game -> IO (G.World, Delta, WorldManager)
mkGameWorld tiledMap startPos = do
	let (worldManager, worldDelta) = execRWS (
			W.stepWire initWire (W.Timed 0 ()) (Right ())
		) world G.emptyWM

	let world' = G.applyDelta world worldDelta
	return (world, world', worldDelta, worldManager)

	where
		world = G.emptyW 
			{ _wTileBoundary = tiledMap^.T.mapTileSize
			}

		initWire = proc input -> do
			p1Id <- spawnObjectAt "Player1" startPos -< input
			_ <- wLiftSetOnce setBoundary G.playerBoundary -< p1Id

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
