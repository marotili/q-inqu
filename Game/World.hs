{-# LANGUAGE FlexibleContexts, TemplateHaskell, Arrows, NamedFieldPuns, Rank2Types #-}
module Game.World 
	(
	-- * World
	  WorldWire, WorldSession, WorldContext
	, World, WorldManager, WorldDelta

	-- * To remove
	, applyDelta

	, newWorldFromTiled
	, testwire
	, updateTiled
	) where

import Debug.Trace
import Control.Concurrent
import Game.World.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Game.Input.Input

import qualified Data.Binary as B

import Control.Wire
import qualified Control.Wire as W
import Control.Wire.Unsafe.Event
import qualified Prelude as P
import Prelude hiding ((.), until)

import Control.Lens
import qualified Control.Lens as L
import Game.World.Objects
import Game.Collision

import Data.Tiled
import Game.World.Import.Tiled

import Game.Input.Actions as A

import Game.World.Types

import Game.World.Delta
import Game.World.Wires
import Game.World.Common

updateTiled :: World -> WorldDelta -> TiledMap -> TiledMap
updateTiled world delta tiled = newTiled
	where
		newTiled = execState (do
			case playerPos of
				Just (px, py) -> do
					object "Player1".objectPos tiled .= (fromJust playerPos)
					object "Player2".objectPos tiled .= (fromJust player2Pos)
				Nothing -> return ()
			) tiled

		(Just pId) = fmap _objId (world^.findObject "Neira")
		playerPos = world^.objectPosition pId
		--let playerGid = world'^.wObjectAnim pId.animTileGid
		--let boulderPos = world'^.wBoulderPos "Boulder1"

		(Just p2Id) = fmap _objId (world^.findObject "TheGhost")
		player2Pos = world^.objectPosition p2Id

		--layerObj :: Traversal' RenderContext Layer
		--layerObj = mapLayers.traverse._ObjectLayer
		--layerObj1 :: Traversal' RenderContext [Layer]
		object name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.objectsByName name



newWorldFromTiled :: TiledMap -> IO (World, WorldManager) -- io due to debug wire
newWorldFromTiled tiledMap = do
	let world = emptyW 
		{ _wTileBoundary = tiledMap^.mapTileSize
		}
	(worldManager, worldDelta) <- execRWST (
			stepWire initWire (Timed 0 ()) (Right ())
		) world emptyWM

	--print worldDelta

	let world' = applyDelta world worldDelta
	--print world'
	return (world', worldManager)

	where
		Just player1Obj = queryObject tiledMap "Player1"
		Just player2Obj = queryObject tiledMap "Player2"
		Just dinoObj = queryObject tiledMap "Dino"
		Just beeObj = queryObject tiledMap "Bee"

		wallPositions = mapWallPositions tiledMap
		boulders = mapBoulders tiledMap

		--genWalls :: [(Float, Float)] -> WorldWire a b
		genWalls [] = returnA
		genWalls (wallPos:walls) = proc input -> do
			spawnObjectAt "Wall" wallPos -< input
			genWalls walls -< input
			returnA -< input

		genBoulders [] = returnA
		genBoulders (boulder:boulders) = proc input -> do
			spawnObjectAt "Boulder" (boulder^.objectPos tiledMap) -< input
			genBoulders boulders -< input
			returnA -< input

		initWire = proc input -> do
			_ <- spawnObjectAt "Neira" (player1Obj^.objectPos tiledMap) -< input
			_ <- spawnObjectAt "TheGhost" (player2Obj^.objectPos tiledMap) -< input
			_ <- spawnObjectAt "Dino" (dinoObj^.objectPos tiledMap) -< input
			_ <- spawnObjectAt "Bee" (beeObj^.objectPos tiledMap) -< input

			-- Initialize: need maybe check in client to remove this TODO
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 1
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 2
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 3
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 4

			_ <- genWalls wallPositions -< input
			_ <- genBoulders boulders -< input

			returnA -< ()

--applyDelta :: World -> WorldDelta -> World
--applyDelta w wd = w --collisions
	--where
	--	newWalls = w { _wWalls = foldr (\d -> Map.insert (wallId d) d) (_wWalls w) (_wdWallsAdd wd) }
	--	newObjects = newWalls & wObjects .~ foldr (\o -> Map.insert (o^.objId) o) (w^.wObjects) (wd^.wdObjectsAdd)
	--	newPlayers = newObjects { _wPlayers = foldr (\p -> Map.insert (playerId p) p) (_wPlayers newObjects) (_wdPlayerAdd wd) }
	--	newBoulders = newPlayers & wBoulders %~ (\boulderMap-> foldr (\b -> Map.insert (boulderId b) b) boulderMap (wd^.wdBouldersAdd))
	--	positions = newBoulders { _wPositions = foldr (\(k, v) m -> Map.alter (alterPos v) k m) (_wPositions newBoulders) (Map.toList . deltaPos . _wdPositionsDelta $ wd) }
		
	--	physics = positions & wPhysics %~ (\physicsMap -> foldr (\(k, v) -> Map.alter (alterPhysics v) k) physicsMap (Map.toList . containerData $ wd^.wdPhysicsDelta))
	--	--positions2 = positions & wPositions = foldr (\objectPhysics ->
	--		--Map.alter (alterPos ))
	--	collidables = physics { _wCollisionManager = execState (do
	--			mapM_ cmAddStatic [newCollidable oId (newBoundary (objectPos oId) (w^.wTileBoundary._1)) | oId <- map wallId (_wdWallsAdd wd)]
	--			mapM_ cmAddFloating [newCollidable oId (newBoundary (objectPos oId) (w^.wTileBoundary._1)) | oId <- map playerId (_wdPlayerAdd wd)]
	--		) (_wCollisionManager physics) }
	--	floatingCollidables = collidables { _wCollisionManager = execState (
	--			mapM_ ((\oId -> cmUpdateFloating oId (objectPos oId)) . fst) (Map.toList (deltaPos $ wd^.wdPositionsDelta))
	--		) (_wCollisionManager collidables) }

	--	anims = floatingCollidables & wAnimations .~ Map.union (wd^.wdAnimations) (w^.wAnimations) -- left-biased
	--	cb = anims & wCollisionCallbacks .~ Map.union (wd^.wdCollisionCallbacks) (anims^.wCollisionCallbacks)

	--	collisionEvents = cb & wCollisionEvents .~ foldr (\(k, v) m -> Map.alter (colAdd [v]) k m) Map.empty (wd^.wdCollisionEvents) 


	--	-- overwrite old ones
	--	collisions = collisionEvents { _wCurrentCollisions = containerData $ wd^.wdCollisions }

	--	objectPos oId = (positions ^. wPositions) Map.! oId

	--	alterPhysics op Nothing = Just op
	--	alterPhysics (ObjectPhysics a1 v1) (Just (ObjectPhysics a2 v2)) = Just $ ObjectPhysics a1 (v1 `mappend` v2)
	--	--doorControllers = positions { wDoorControllers = foldr (\dc -> Map.insert (doorControllerId dc) dc) (wDoorControllers positions) (wdDoorControllers wd) }

moveArrow = proc oId -> do
	_ <- move (0, 100) . for 2 -< oId
	returnA -< ()

spawnArrow = spawn . thenDo (inhibit WireFinished)
	where
		spawn = proc input -> do	
			oId <- spawnObjectAt "Arrow" (50, 50) -< input
			_ <- newObjectWireR moveArrow -< oId
			returnA -< ()

playerSpawnArrow = untilV spawnArrowEvent
	W.--> spawnArrow 
	W.--> void while . spawnArrowEvent
	W.--> playerSpawnArrow

playerMovement = untilV movingDirectionE 
	W.--> move
	W.--> playerMovement
	where
		move = proc pId -> do
			(dx, dy) <- movingDirectionR -< pId
			_ <- moveR -< (pId, (-dx*200, dy*200))
			returnA -< ()

playerWire :: ObjectWire ObjectId ()
playerWire = proc pId -> do
	_ <- playerSpawnArrow -< pId
	_ <- playerMovement -< pId
	returnA -< ()

testwire :: WorldWire a ()
testwire = proc input -> do
	_ <- stepObjectWires -< input
	_ <- once . newObjectWire 1 playerWire -< input
	_ <- once . newObjectWire 2 playerWire -< input
	--_ <- deaccelObjects -< input
	--_ <- moveObjects -< input

	--playerId <- player "Neira" -< input
	--player2Id <- player "TheGhost" -< input
	--dinoId <- player "Dino" -< input
	--beeId <- player "Bee" -< input
	--boulderId <- boulder "Boulder1" -< input

	--_ <- movement -< playerId
	--_ <- movement -< player2Id

	--_ <- animate dinoAnim -< dinoId
	--Just (x, y) <- liftW $ asks (\w -> w^.wPlayerPos "Neira") -< input
	--Just (x', y') <- liftW $ asks (\w -> w^.wPlayerPos "Dino") -< input
	--_ <- moveObjectR -< (dinoId, (userSpeed / 1.5 * norm (x - x'), userSpeed / 1.5 * norm (y - y')))

	--_ <- liftW $ deltaSetCollisionCb (\oId w -> w) 1 -< input

	--_ <- void (for 1) W.--> animate beeAnim -< beeId

	--_ <- colLoop -< playerId

	--_ <- spawnArrow -< playerId
	--_ <- playerWire -< 1
	returnA -< ()
	where
		--movement = W.until . (fmap (\e ->  ((), e))) movingDirectionR 
		--	W.--> playerMovement 
		--	W.--> inhibit () . playerResetAnimation
		--	W.--> movement

worldLoop w' session' world' state' = do
	(dt, session) <- stepSession session'
	((out, w), worldManager, worldDelta) <- runRWST (
		stepWire w' dt (Right ())
		) world' state'

	let quit = case out of
		Right _ -> False
		Left _ -> True
	return (quit, (w, session), (worldManager, worldDelta))
