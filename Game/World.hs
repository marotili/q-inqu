{-# LANGUAGE BangPatterns, FlexibleContexts, Arrows, NamedFieldPuns, Rank2Types #-}
module Game.World 
	(
	-- * World
	  WorldWire, WorldSession, WorldContext
	, World, WorldManager, WorldDelta

	-- * To remove
	--, newWorldFromTiled
	, testwire
	) where

import Game.World.ObjectData
import Debug.Trace
import Game.World.Lens
import Game.World.Unit
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
	_ <- waitHit W.--> removeArrow -< oId
	returnA -< ()

	where
		waitHit = proc oId -> do
			_ <- move direction . for 1 -< oId
			_ <- untilV (wLiftE objectCollided) W.-->
					removeArrow . handleCollision
				 -< oId
			returnA -< ()

		removeArrow = untilV removeObject W.--> void exit

		handleCollision = proc oId -> do
			collisions <- asSoonAs . (wLiftE objectCollided) -< oId
			unitCollisions <- wLiftF (\cols -> do
					world <- ask
					return $ filter (\oId -> world^.isUnit oId) cols
				)-< collisions

			_ <- newObjectWiresR -< (unitCollisions, stun)
			returnA -< traceShow (collisions, unitCollisions) oId

stun :: ObjectWire ObjectId ()
stun = proc oId -> do
	_ <- move (-50, -50) . for 1 W.--> void exit -< oId	
	returnA -< ()

objectCollided oId = do
	collisions <- view (collisionEvent oId)
	if null collisions
		then return (False, [])
		else return (True, collisions)

data Arrow = Arrow
instance Spawnable Game.World.Arrow where
	spawnPosition Arrow = proc playerId -> do
		Just playerPos <- wLiftF (\pId -> view $ getPositions . L.at pId) -< playerId
		returnA -< playerPos
	spawnWire _ = proc playerId -> do
		(dx, dy) <- spawnArrowDirection -< playerId
		let wire = moveArrow (dx*400, dy*400)
		returnA -< wire
	spawnBoundary _ = proc playerId -> do
		(dx, dy) <- spawnArrowDirection -< playerId
		let rotation = if
			dy > 0 
				then acos dx
				else -(acos dx)
		returnA -< arrowData^.bdBoundary rotation

	spawnAnimation _ = pure $ arrowAnimation East


class Spawnable a where
	spawnPosition :: a -> WorldWire PlayerId (Float, Float)
	spawnRotation :: a -> WorldWire PlayerId Float
	spawnRotation _ = pure 0
	spawnWire :: a -> WorldWire PlayerId (WorldWire ObjectId ())

	spawnIgnoreList :: a -> WorldWire PlayerId [ObjectId]
	spawnIgnoreList _ = fmap (:[]) W.id
	spawnIgnoreSpawner :: a -> WorldWire PlayerId Bool
	spawnIgnoreSpawner _ = pure True
	spawnBoundary :: a -> WorldWire PlayerId Boundary
	spawnBoundary _ = pure []

	spawnAnimation :: a -> WorldWire PlayerId Animation
	--spawnAnimation _ = pure (arrowAnimation East) -- FIXME need empty animation

	spawnObjectWire :: a -> WorldWire PlayerId ()
	spawnObjectWire obj = proc spawnerId -> do
		Event oId <- spawnObjectMakeName -< spawnerId
		position <- spawnPosition obj -< spawnerId
		rotation <- spawnRotation obj -< spawnerId
		ignoreList <- spawnIgnoreList obj -< spawnerId
		ignoreSpawner <- spawnIgnoreSpawner obj -< spawnerId
		boundary <- spawnBoundary obj -< spawnerId
		wire <- spawnWire obj -< spawnerId
		anim <- spawnAnimation obj -< spawnerId

		_ <- animateR -< (oId, anim)
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
spawnArrow = spawn' . thenDo (inhibit WireFinished)
	where
		spawn' = spawnObjectWire Arrow

			--proc playerId -> do	
			--(oId, playerPos, playerDir) <- step1 -< playerId
			--_ <- setup -< (oId, playerId, playerPos, playerDir, playerId)
			--_ <- spawnWire -< (playerId, oId, playerDir)

			--returnA -< ()

		--step1 = proc playerId -> do
		--	Event oId <- spawnObjectMakeName -< playerId
		--	Just playerPos <- wLiftF (\pId -> view $ getPositions . L.at pId) -< playerId
		--	Just playerDir <- wLiftF (\pId -> view $ getOrientations . L.at pId) -< playerId
		--	returnA -< (oId, playerPos, playerDir)

		--spawnWire = proc (pId, oId, playerDir) -> do
		--	--let (dx, dy) = deltaFromOrientation playerDir
		--	(dx, dy) <- spawnArrowDirection -< pId
		--	_ <- wLiftSetOnceR rotateObject -< (oId, if
		--		dy > 0 
		--			then acos dx
		--			else -(acos dx)
		--		)
		--	let wire = moveArrow (dx*400, dy*400)
		--	_ <- newObjectWireR -< (oId, wire)
		--	returnA -< ()

		--setup = proc (oId, pId, playerPos, playerDir, playerId) -> do
		--	(dx, dy) <- spawnArrowDirection -< pId
		--	let rotation = if
		--		dy > 0 
		--			then acos dx
		--			else -(acos dx)

		--	_ <- setPosOnceR -< (oId, playerPos)
		--	_ <- animateR -< (oId, arrowAnimation East)
		--	_ <- wLiftSetOnceR setBoundary -< (oId, arrowData^.bdBoundary rotation)
		--	_ <- wLiftSetOnceR setIgnoreCollision -< (oId, playerId)
		--	_ <- wLiftSetOnceR setIgnoreCollision -< (playerId, oId)
			--returnA -< ()

playerSpawnArrow :: ObjectWire PlayerId ()
playerSpawnArrow = untilV spawnArrowEvent
	W.--> spawnArrow 
	W.--> void while . spawnArrowEvent
	W.--> playerSpawnArrow

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
