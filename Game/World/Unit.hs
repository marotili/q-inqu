{-# LANGUAGE TemplateHaskell, Arrows #-}
module Game.World.Unit where

import Game.World.Common
import Game.World.Objects
import Control.Lens
import qualified Data.Map as Map



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
            units <- lift catMaybes . lift unit . collided -< forceObjId
            let eligibleUnits = filter (not . effectImmune) units

            --_ <- spawnWires -< affectedUnits

            returnA -< ()

unit :: ObjectId -> WorldContext (Maybe Unit)
unit oId = view $ getUnits . at oId

isUnit :: ObjectId -> WorldContext Bool
isUnit oId = do
    unit <- view $ getUnits . at oId 
    return $ unit /= Nothing

effectImmune :: Unit -> Bool
effectImmune unit = Set.member ConditionEffectImmunity (unit^.unitConditions) 


