{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Game.World.Wires where

import qualified Data.Map as Map
import Game.World.Objects
import qualified Game.World.Delta as World
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Lens 
import qualified Control.Lens as L
import Linear
--import Game.World.Types
import Game.Collision
import Control.Monad.State
import Control.Monad
import Control.Wire
import qualified Control.Wire as W
import qualified Control.Wire.Unsafe.Event as W
import Game.World.Common
import Data.Maybe
import qualified Game.Input.Actions as A
import Prelude hiding ((.))
import qualified Prelude as P

stepObjectWires  :: WorldWire a ()
stepObjectWires = mkGen $ \ds a -> do
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
					World.addWire oId [w']
				Left _ -> return ()

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
		Nothing -> return (Left (), wLiftM f)

--objectR :: WorldWire ObjectId (Maybe Object)
--objectR = mkGenN $ \oId -> do
--	obj <- view $ World.getObjects . L.at oId
--	return (Right obj, objectR)

--objectRI :: WorldWire ObjectId Object
--objectRI = mkGenN $ \oId -> do
--	obj <- view $ World.getObjects . L.at oId
--	return $ case obj of
--		Just o -> (Right o, objectRI)
--		Nothing -> (Left (), objectRI)

_move ds oId (vx, vy) = do
	let dt = realToFrac (dtime ds)
	let (dx, dy) = (dt * vx, dt * vy)
	World.moveObject oId (dx, dy)

move :: (Float, Float) -> WorldWire ObjectId ()
move speed = mkGen $ \ds oId -> do
	_move ds oId speed
	return (Right (), move speed)

setPos pos = mkGenN $ \oId -> do
	World.moveObject oId pos
	return (Right (W.Event ()), never)

moveR :: WorldWire (ObjectId, (Float, Float)) ()
moveR = mkGen $ \ds (oId, speed) -> do
	_move ds oId speed
	return (Right (), moveR)

newObject :: MonadState WorldManager m => m ObjectId
newObject = do
	wm <- get
	let oId = wm^.wmNextObjectId
	wmNextObjectId += 1
	return oId

spawnObject :: String -> WorldWire a (Event ObjectId)
spawnObject name = mkGenN $ \_ -> do
	oId <- newObject
	World.addObject oId (Object oId name)
	return (Right (W.Event oId), never)

spawnObjectAt :: String -> (Float, Float) -> WorldWire a ()
spawnObjectAt name pos = 
	inhibit () . setPos pos . asSoonAs . spawnObject name

movingDirection :: WorldWire PlayerId (W.Event (Float, Float))
movingDirection = mkGenN $ \playerId -> do
	wm <- get
	if Map.member playerId (wm^.wmPlayerActions)
		then do
			let playerActions = asks _wmPlayerActions wm Map.! playerId
			let direction = A.movingDirection playerActions
			case direction of
				(0, 0) -> do
					return (Right W.NoEvent, movingDirection)
				_ -> do
					return (Right (W.Event direction), movingDirection)
		else
			return (Right W.NoEvent, movingDirection)

animate :: Animation -> WorldWire ObjectId ()
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

animateR :: WorldWire (Animation, ObjectId) ()
animateR = mkGen $ \ds (animation, oId) -> do
	mObjAnim <- view (World.getAnimations P.. L.at oId)
	case mObjAnim of
		Nothing -> do
			(mx, w') <- stepWire (animate animation) ds (Right oId)
			return (mx, animateR)
		Just oAnim ->
			if oAnim == animation 
				then do
					-- w' is is animate plus the updated animation
					-- we can drop it since on the next invocation we pass the new state using wObjectName
					(mx, w') <- stepWire (animate oAnim) ds (Right oId)
					return (mx, animateR)
				else do
					(mx, w') <- stepWire (animate animation) ds (Right oId)
					return (mx, animateR)