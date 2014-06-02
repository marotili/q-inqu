{-# LANGUAGE FlexibleContexts, Rank2Types, TemplateHaskell, Arrows #-}
module Engine.World.Unit where

import Control.Monad.Writer
import Engine.World.Common
import Engine.World.Objects
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Engine.World.Lens
import Data.Maybe

unit :: ObjectId -> WorldContext (Maybe Unit)
unit oId = view $ getUnits . at oId

effectImmune :: Unit -> Bool
effectImmune unit = Set.member ConditionEffectImmunity (unit^.unitConditions) 

newEquipment = Equipment
    { _equipmentSlots = Map.empty
    } 
    & equipmentSlots . at ESTorso .~ (Just Nothing)
    & equipmentSlots . at ESPrimaryWeapon .~ (Just Nothing)
    & equipmentSlots . at ESSecondaryWeapon .~ (Just Nothing)

newInventory = Inventory
    { _invItemInstances = Map.empty
    , _invMaxItems = 10
    }

newUnitHealth = UnitHealth
    { _uhMax = 10
    , _uhCurrent = 10
    }

newUnit oId = Unit
    { _unitId = oId
    , _unitHealth = newUnitHealth
    , _unitEquipment = newEquipment
    , _unitInventory = newInventory
    , _unitAbilities = Map.empty
    , _unitConditions = Set.empty
    }

setUnits :: Set (ModifyContainer UnitId Unit)
setUnits = _compSet compUnit
getUnits :: Get (ObjectProp Unit)
getUnits = _compGet compUnit

makeUnit :: (MonadWriter WorldDelta m) => ObjectId -> m ()
makeUnit oId = writeProp setUnits oId (ModifySet $ newUnit oId)

unitObjects :: Get [ObjectId]
unitObjects = to (\w -> Map.keys $ w^.getUnits)

isUnit :: ObjectId -> Get Bool
isUnit oId = to (\w -> oId `elem` (w^.unitObjects))
