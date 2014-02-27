{-# LANGUAGE FlexibleContexts, Arrows, NamedFieldPuns, Rank2Types #-}
module Game.World 
	(
	-- * World
	  WorldWire, WorldSession, WorldContext
	, World, WorldManager, WorldDelta

	-- * To remove
	--, newWorldFromTiled
	, testwire
	) where

import Debug.Trace
import Game.World.Lens
import Control.Monad.RWS
import Control.Monad.State
import Data.Maybe
import Game.Input.Input

import Control.Wire
import qualified Control.Wire as W
import Control.Wire.Unsafe.Event
import qualified Prelude as P
import Prelude hiding ((.), until)

import Control.Lens
import qualified Control.Lens as L
import Game.World.Objects

import qualified Data.Tiled as T
import Data.Tiled
import Game.World.Import.Tiled


import Game.World.Delta
import Game.World.Wires
import Game.World.Common
import qualified Game.World.Objects as World

moveArrow :: (Float, Float) -> ObjectWire ObjectId ()
moveArrow direction = proc oId -> do
	_ <- move direction . for 1 
		W.--> untilV removeObject W.--> void exit
			-< oId
	returnA -< ()

data Arrow = Arrow
instance Spawnable Game.World.Arrow where
	spawnPosition Arrow = proc playerId -> do
			Just playerPos <- wLiftF (\pId -> view $ getPositions . L.at pId) -< playerId
			returnA -< playerPos

class Spawnable a where
	spawnPosition :: a -> WorldWire PlayerId (Float, Float)
	spawnRotation :: a -> WorldWire PlayerId Float
	spawnRotation _ = pure 0
	spawnWire :: a -> WorldWire PlayerId (WorldWire ObjectId ())

	spawnIgnoreList :: a -> WorldWire PlayerId [ObjectId]
	spawnIgnoreList _ = fmap (\pId -> [pId]) W.id
	spawnIgnoreSpawner :: a -> WorldWire PlayerId Bool
	spawnIgnoreSpawner _ = pure $ True
	spawnBoundary :: a -> WorldWire PlayerId Boundary
	spawnBoundary _ = pure []

	spawn :: a -> WorldWire PlayerId ()

	spawn obj = proc spawnerId -> do
		Event oId <- spawnObjectMakeName -< spawnerId
		position <- spawnPosition obj -< spawnerId
		rotation <- spawnRotation obj -< spawnerId
		ignoreList <- spawnIgnoreList obj -< spawnerId
		ignoreSpawner <- spawnIgnoreSpawner obj -< spawnerId
		boundary <- spawnBoundary obj -< spawnerId
		wire <- spawnWire obj -< spawnerId

		_ <- setPosOnceR -< (oId, position)
		_ <- wLiftSetOnceR setBoundary -< (oId, boundary)
		_ <- wLiftSetOnceR setIgnoreCollision -< (oId, head ignoreList) -- FIXME for all in list

		_ <- wLiftSetOnceR setIgnoreCollision -< (if
					ignoreSpawner then spawnerId else oId, oId)

		_ <- newObjectWireR -< (oId, wire)
		returnA -< ()

--ignore = proc (oId, playerId) -> do
--	_ <- wLiftSetOnceR setIgnoreCollision -< (oId, playerId)
--	_ <- wLiftSetOnceR setIgnoreCollision -< (playerId, oId)
--	returnA -< ()

boltWire = proc boltId -> do
	_ <- move (20, 20) . for 1 
		W.--> untilV removeObject W.--> void exit
			-< boltId
	_ <- animate boltAnimation -< boltId
	returnA -< ()

instance Spawnable ItemInstance where
	spawnPosition itemInstance = proc playerId -> do
			Just playerPos <- wLiftF (\pId -> view $ getPositions . L.at pId) -< playerId
			returnA -< playerPos

	spawnWire itemInstance = proc playerId -> do
		Just item <- wLift (view $ getItems . L.at (itemInstance ^. iiItemId)) -< playerId
		name <- wLift (view $ getObjects . L.at (itemInstance ^. iiItemId) . _Just . objName) -< playerId
		returnA -< case name of
			"Bolt" -> boltWire
			_ -> boltWire

spawnArrow :: ObjectWire PlayerId ()
spawnArrow = spawn . thenDo (inhibit WireFinished)
	where
		spawn = proc playerId -> do	
			(oId, playerPos, playerDir) <- step1 -< playerId
			_ <- setup -< (oId, playerId, playerPos, playerDir, playerId)
			_ <- spawnWire -< (playerId, oId, playerDir)

			returnA -< ()

		step1 = proc playerId -> do
			Event oId <- spawnObjectMakeName -< playerId
			Just playerPos <- wLiftF (\pId -> view $ getPositions . L.at pId) -< playerId
			Just playerDir <- wLiftF (\pId -> view $ getOrientations . L.at pId) -< playerId
			returnA -< (oId, playerPos, playerDir)

		spawnWire = proc (pId, oId, playerDir) -> do
			--let (dx, dy) = deltaFromOrientation playerDir
			(dx, dy) <- spawnArrowDirection -< pId
			_ <- wLiftSetOnceR rotateObject -< (oId, if
				dy > 0 
					then acos dx
					else -(acos dx)
				)
			let wire = moveArrow (dx*400, dy*400)
			_ <- newObjectWireR -< (oId, wire)
			returnA -< ()

		setup = proc (oId, pId, playerPos, playerDir, playerId) -> do
			(dx, dy) <- spawnArrowDirection -< pId
			let rotation = if
				dy > 0 
					then acos dx
					else -(acos dx)

			_ <- setPosOnceR -< (oId, playerPos)
			_ <- animateR -< (oId, arrowAnimation East)
			_ <- wLiftSetOnceR setBoundary -< (oId, arrowData^.bdBoundary rotation)
			_ <- wLiftSetOnceR setIgnoreCollision -< (oId, playerId)
			_ <- wLiftSetOnceR setIgnoreCollision -< (playerId, oId)
			returnA -< ()

playerSpawnArrow :: ObjectWire PlayerId ()
playerSpawnArrow = untilV spawnArrowEvent
	W.--> spawnArrow 
	W.--> void while . spawnArrowEvent
	W.--> playerSpawnArrow

--swordTrackPlayer :: ObjectWire PlayerId ()
--swordTrackPlayer = 


--playerMovement :: ObjectWire PlayerId ()
playerMovement baseSpeed = untilV movingDirectionE 
	W.--> movePlayer
	W.--> playerMovement baseSpeed
	where
		movePlayer = proc pId -> do
			-- Direction of player
			(dx, dy) <- movingDirectionR -< pId

			-- move the player
			_ <- moveR -< (pId, (-dx*baseSpeed, dy*baseSpeed))

			-- set new orientation
			let orientation = orientationFromDelta (-dx, dy)
			_ <- wLiftUpdateR setOrientation -< (pId, orientation)

			-- animate the player
			let anim = objectAnimation pId orientation
			_ <- animateR -< (pId, anim)
			returnA -< ()

playerWire :: ObjectWire ObjectId ()
playerWire = proc pId -> do
	_ <- playerSpawnArrow -< pId
	_ <- playerMovement 300 -< pId
	returnA -< ()

dinoWire :: ObjectWire ObjectId ()
dinoWire = proc pId -> do
	_ <- animate (objectAnimation 3 North) -< pId
	_ <- playerMovement 100 -< 3
	returnA -< ()

beeWire :: ObjectWire ObjectId ()
beeWire = proc pId -> do
	_ <- animate (objectAnimation 4 North) -< pId
	_ <- playerMovement 400 -< 4
	returnA -< ()

testwire :: WorldWire a ()
testwire = proc input -> do
	_ <- stepObjectWires -< input
	_ <- once . newObjectWire 1 playerWire -< input
	_ <- once . newObjectWire 2 playerWire -< input
	_ <- once . newObjectWire 3 dinoWire -< input
	_ <- once . newObjectWire 4 beeWire -< input

	returnA -< ()


--worldLoop w'a session' world' state' = do
--	(dt, session) <- stepSession session'
--	((out, w), worldManager, worldDelta) <- runRWST (
--		stepWire w' dt (Right ())
--		) world' state'

--	let quit = case out of
--		Right _ -> False
--		Left _ -> True
--	return (quit, (w, session), (worldManager, worldDelta))
