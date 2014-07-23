{-# LANGUAGE TemplateHaskell, Arrows #-}
module Game.World.UnitWires 
(
)
where

import Game.World.Common
import Game.World.Objects
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Wire
import qualified Control.Wire as W
import Game.Input.Input
import Game.World.Objects
import Game.World.Common
import Game.World.Unit
import Game.World.Wires
import Prelude hiding ((.))


---------------------------------------------------------
-- World
---------------------------------------------------------


--filterWire :: (a -> WorldWire a b) -> WorldWire c [a] -> WorldWire c [b]

pushWire :: WorldWire ObjectId ()
pushWire = proc unitId -> do
    -- spawn force object with collidable component
    -- set collide callback
    --  on collide : stun and pushback
    returnA -< ()

    where
        forceWire direction = proc forceObjId -> do
            _ <- for 2 . move direction -< forceObjId
            _ <- loop -< forceObjId
            returnA -< ()

        loop = untilV collisionEvent W.--> test W.--> loop

        test = proc forceObjId -> do
            units <- lift catMaybes . lift isUnit . collided -< forceObjId
            let eligibleUnits = filter (not . effectImmune) units

            --_ <- spawnWires -< affectedUnits

            returnA -< ()
