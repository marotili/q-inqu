{-# LANGUAGE FlexibleContexts, Arrows, Rank2Types, FlexibleInstances #-}
module Game.World.Wires where

import qualified Data.Map as Map
import Debug.Trace
import qualified Data.Set as Set
import Game.World.Objects
import qualified Game.World.Lens as World
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Lens 
import qualified Control.Lens as L
--import Game.World.Types
import Game.Collision
import Control.Monad.State
import Control.Monad
import Control.Wire
import qualified Control.Wire as W
import qualified Control.Wire.Unsafe.Event as W
import Game.World.Common
import Game.World.Lens
import Data.Maybe
import qualified Game.Input.Actions as A
import Prelude hiding ((.))
import Linear
import qualified Prelude as P

--instance Par.MonadParallel (RWST World WorldDelta WorldManager IO)
	--bindM2 :: (a -> b -> m c) -> m a -> m b -> m c
	--bindM2 f ma mb = let ma' = ma >>= return
 --                        mb' = mb >>= return
 --                    in ma' `par` (mb' `pseq` do {a <- ma'; b <- mb'; f a b})

--parMapM_ :: Par.MonadParallel m => (a -> m b) -> [a] -> m ()
--parMapM_ f list = Par.sequence_ (map f list)

stepObjectWires  :: WorldWire a ()
stepObjectWires = mkGen $ \ds _ -> do
	wires <- view World.getWires
	mapM_ (uncurry (stepObjectWire ds)) $ Map.toList wires
	return (Right (), stepObjectWires)

	where
		stepObjectWire ds oId =
			mapM_ (stepObjectW oId ds)

		stepObjectW oId ds w = do
			(mx, w') <- stepWire w ds (Right oId)
			case mx of
				Right _ ->
					World.addWire oId w'
				Left i -> case i of
					WireRunning ->
						World.addWire oId w'
					WireFinished ->
						return ()

exit :: WorldWire a a
exit = inhibit WireFinished

wLiftUpdateR :: (ObjectId -> a -> WorldContext b) -> WorldWire (ObjectId, a) ()
wLiftUpdateR f = mkGenN $ \(oId, a) -> do
	_ <- f oId a
	return (Right (), wLiftUpdateR f)

wLiftSetOnceR :: (ObjectId -> a -> WorldContext b) -> WorldWire (ObjectId, a) (Event ())
wLiftSetOnceR f = mkGenN $ \(oId, a) -> do
	_ <- f oId a
	return (Right (W.Event ()), never)

wLiftSetOnce :: (ObjectId -> a -> WorldContext b) -> a -> WorldWire ObjectId (Event ())
wLiftSetOnce f a = mkGenN $ \oId -> do
	_ <- f oId a
	return (Right (W.Event ()), never)

wLiftSetOnceVoid :: (ObjectId -> WorldContext b) -> WorldWire ObjectId (Event ())
wLiftSetOnceVoid f = mkGenN $ \oId -> do
	_ <- f oId
	return (Right (W.Event ()), never)

wLiftF :: (a -> WorldContext b) -> WorldWire a b
wLiftF f = mkGenN $ \a -> do
	b <- f a
	return (Right b, wLiftF f)

wLift :: WorldContext b -> WorldWire a b
wLift a = wLiftF (const a)

wLiftM :: (a -> WorldContext (Maybe b)) -> WorldWire a b
wLiftM f = mkGenN $ \a -> do
	mb <- f a
	case mb of
		Just b -> return (Right b, wLiftM f)
		Nothing -> return (Left WireRunning, wLiftM f)

-- TODO: We should move the test into the state monad
-- e.g.: object moves -> no collision and has new position in next update
-- 		 	another object moves -> no collision but may overlap in the next update
-- 			the collision manager should always know about the future positions of the objects
collides :: ObjectId -> (Float, Float) -> WorldContext [ObjectId]
collides oId (dx, dy) = do
	cm <- view wCollisionManager
	--Just oldPos <- view $ wCommon.wcPositions.at oId
	boundary <- view $ objectBoundary oId
	let newObjBoundary = map (\(x, y) -> (x + dx, y + dy)) boundary
	let collisions = evalState (do
		octreeUpdate [(oId, newObjBoundary)]
		octreeQueryObject oId
		) cm
	return collisions

_move :: Timed NominalDiffTime () -> ObjectId -> (Float, Float) -> WorldContext ()
_move ds oId (vx, vy) = do
	canCollide <- view $ isCollidable oId
	mignore <- view $ getCollisionFilters . L.at oId
	let dt = realToFrac (dtime ds)
	let (dx, dy) = (dt * vx, -dt * vy)
	if canCollide  
		then do
			collisions <- collides oId (dx, dy)
			let realCollisions = case mignore of
				Just ignore ->	filter (\col -> not (Set.member col ignore)) collisions
				Nothing -> collisions
			if (null realCollisions) 
				then
					World.moveObject oId (dx, dy)
				else
					mapM_ (setCollisionEvent oId) realCollisions

		else
			World.moveObject oId (dx, dy)

move :: (Float, Float) -> ObjectWire ObjectId ()
move speed = mkGen $ \ds oId -> do
	_move ds oId speed
	return (Right (), move speed)

setPosOnce :: (Float, Float) -> WorldWire ObjectId (Event ())
setPosOnce = wLiftSetOnce World.moveObject
setPosOnceR :: WorldWire (ObjectId, (Float, Float)) (Event ())
setPosOnceR = wLiftSetOnceR World.moveObject
--mkGenN $ \oId -> do
--	World.moveObject oId pos
--	return (Right (W.Event ()), never)

moveR :: ObjectWire (ObjectId, (Float, Float)) ()
moveR = mkGen $ \ds (oId, speed) -> do
	_move ds oId speed
	return (Right (), moveR)

newObject :: MonadState WorldManager m => m ObjectId
newObject = do
	wm <- get
	let oId = wm^.wmNextObjectId
	wmNextObjectId += 1
	return oId

newObjectWire :: ObjectId -> ObjectWire ObjectId () -> WorldWire a (Event ())
newObjectWire oId w = doOnce $ wLift (addWire oId w)

newObjectWireR :: WorldWire (ObjectId, ObjectWire PlayerId ()) (Event ())
newObjectWireR = newObjectWireR' newObjectWire 
	where
		newObjectWireR' newObjectW = mkGen $ \ds (oId, w) -> do
			(mx, w') <- stepWire (newObjectW oId w) ds (Right ())
			return (mx, newObjectWireR' (\_ _ -> w'))

spawnObject :: String -> WorldWire a (Event ObjectId)
spawnObject name = mkGenN $ \_ -> do
	oId <- newObject
	World.addObject oId (Object oId name)
	return (Right (W.Event oId), never)

removeObject :: WorldWire ObjectId (Event ())
removeObject = mkGenN $ \oId -> do
	World.deleteObject oId
	return (Right (W.Event ()), never)

spawnObjectMakeName :: WorldWire a (Event ObjectId)
spawnObjectMakeName = mkGenN $ \_ -> do
	oId <- newObject
	World.addObject oId (Object oId (show oId))
	return (Right (W.Event oId), never)

spawnObjectAt :: String -> (Float, Float) -> WorldWire a ObjectId
spawnObjectAt name pos = proc input -> do
	W.Event oId <- spawnObject name -< input
	_ <- setPosOnce pos -< oId
	returnA -< oId

doOnce :: WorldWire a b -> WorldWire a (Event b)
doOnce w = mkGen $ \ds a -> do
	(mx, _) <- stepWire w ds (Right a)
	return $ case mx of
		Right b -> (Right (W.Event b), never)	
		Left x -> (Left x, never)

thenDo :: WorldWire a a -> WorldWire a a
thenDo w = mkGenN $ \a -> return (Right a, w)

while :: WorldWire (Event a) (Event a)
while = W.when (\a ->
		case a of
			W.Event _ -> True
			W.NoEvent -> False
	)

--asLongAs = mkGenN $ \a -> do
	--case a of
		--Event x -> return (Right x, asLongAs)

spawnArrowDirection :: WorldWire PlayerId (Float, Float)
spawnArrowDirection = mkGenN $ \pId -> do
	actions <- get >>= \wm -> return $ wm^.wmPlayerActions
	if Map.member pId actions
		then do
			let (A.InputActions playerActions) = actions Map.! pId
			let as = filter 
				(\a -> case a of A.ActionSpawnArrow {} -> True; _ -> False)
				$ Set.toList playerActions	
			return $ if not (null as)
				then do
					let A.ActionSpawnArrow x y = head as
					let V2 dx dy = normalize (V2 x (-y))
					(Right (dx, dy), spawnArrowDirection)
				else
					(Right (0, 0), spawnArrowDirection)
		else
			return (Right (0, 0), spawnArrowDirection)

spawnArrowEvent :: WorldWire PlayerId (Event ())
spawnArrowEvent = mkGenN $ \pId -> do
	actions <- get >>= \wm -> return $ wm^.wmPlayerActions
	if Map.member pId actions
		then do
			let (A.InputActions playerActions) = actions Map.! pId
			let as = filter 
				(\a -> case a of A.ActionSpawnArrow {} -> True; _ -> False)
				$ Set.toList playerActions
			return $ if not (null as)
				then
					(Right $ W.Event (), spawnArrowEvent)
				else
					(Right W.NoEvent, spawnArrowEvent)
		else
			return (Right W.NoEvent, spawnArrowEvent)

movingDirectionR :: ObjectWire PlayerId (Float, Float)
movingDirectionR = mkGenN $ \playerId -> do
	wm <- get
	if Map.member playerId (wm^.wmPlayerActions)
		then do
			let playerActions = asks _wmPlayerActions wm Map.! playerId
			let direction = A.movingDirection playerActions
			case direction of
				(0, 0) -> 
					return (Left WireRunning, movingDirectionR)
				_ -> 
					return (Right direction, movingDirectionR)
		else
			return (Left WireRunning, movingDirectionR)

movingDirectionE :: ObjectWire PlayerId (W.Event (Float, Float))
movingDirectionE = mkGenN $ \playerId -> do
	wm <- get
	if Map.member playerId (wm^.wmPlayerActions)
		then do
			let playerActions = asks _wmPlayerActions wm Map.! playerId
			let direction = A.movingDirection playerActions
			case direction of
				(0, 0) ->
					return (Right W.NoEvent, movingDirectionE)
				_ ->
					return (Right (W.Event direction), movingDirectionE)
		else
			return (Right W.NoEvent, movingDirectionE)

animate :: Animation -> ObjectWire ObjectId ()
animate anim = mkGen $ \ds oId -> do
	let dt = realToFrac (dtime ds)
	if anim^.animCurrentTime + dt > anim ^. animTime
		then do
			let remaining = realToFrac $ anim^.animCurrentTime + dt - anim^.animTime
			let next = (anim^.animNext) & animCurrentTime .~ 0
			(mx, w') <- stepWire (animate next) (W.Timed (fromRational remaining) ()) (Right oId)
			return (mx, w')
		else do
			let current = anim & animCurrentTime %~ (+) dt
			World.setAnimation oId current
			return (Right (), animate current)

animateR :: ObjectWire (ObjectId, Animation) ()
animateR = mkGen $ \ds (oId, animation) -> do
	mObjAnim <- view (World.getAnimations P.. L.at oId)
	case mObjAnim of
		Nothing -> do
			(mx, _) <- stepWire (animate animation) ds (Right oId)
			return (mx, animateR)
		Just oAnim ->
			if oAnim == animation 
				then do
					-- w' is is animate plus the updated animation
					-- we can drop it since on the next invocation we pass the new state using wObjectName
					(mx, _) <- stepWire (animate oAnim) ds (Right oId)
					return (mx, animateR)
				else do
					(mx, _) <- stepWire (animate animation) ds (Right oId)
					return (mx, animateR)