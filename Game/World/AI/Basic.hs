module Game.World.AI.Basic 
(
)
where

import Game.World.Common
import Game.World.Objects
import Game.World.Lens
import Control.Monad.RWS
import Control.Lens
import Game.Input.Actions
import Linear
import Debug.Trace

data Reactive = Reactive

type MonsterId = PlayerId
followPlayer1AI :: MonsterId -> RWS (World, WorldDelta, Rational) InputActions () ()
followPlayer1AI mId = do
	(world, worldDelta, dt) <- ask
	let pId = 1
	let Just (px, py) = world^.objectPosition pId
	let Just (aix, aiy) = world^.objectPosition mId

	let (dx, dy) = (aix - px, aiy - py)

	writer ((), newInputAction $ newMoveAction dx dy)

keepDistanceAI mId = do
	(world, worldDelta, dt) <- ask
	let pId = 1
	let Just (px, py) = world^.objectPosition pId
	let Just (aix, aiy) = world^.objectPosition mId

	let (dx, dy) = (aix - px, aiy - py)

	writer $ if norm (V2 dx dy)  < 100 then
		((), newInputAction $ newMoveAction (-dx) (-dy))
	else
		((), newInputAction ActionStopMove)

runAI :: MonsterId -> World -> WorldDelta -> Rational -> InputActions
runAI 3 w wd r = let (_, _, ia) = runRWS (followPlayer1AI 3) (w, wd, r) () in ia
runAI 4 w wd r = let (_, _, ia) = runRWS (keepDistanceAI 4) (w, wd, r) () in ia
