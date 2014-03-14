{-# LANGUAGE TemplateHaskell, Arrows #-}
module Game.World.Unit where

import Game.World.Common
import Game.World.Objects
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Game.World.Lens
import Data.Maybe

unit :: ObjectId -> WorldContext (Maybe Unit)
unit oId = view $ getUnits . at oId

isUnit :: ObjectId -> WorldContext Bool
isUnit oId = do
    unit <- view $ getUnits . at oId 
    return . isNothing $ unit

effectImmune :: Unit -> Bool
effectImmune unit = Set.member ConditionEffectImmunity (unit^.unitConditions) 


