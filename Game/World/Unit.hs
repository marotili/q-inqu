{-# LANGUAGE TemplateHaskell, Arrows #-}
module Game.World.Unit where

import Game.World.Common
import Game.World.Objects
import Control.Lens
import qualified Data.Map as Map

type InventoryId = Int
type ItemId = Int
type ItemInstanceId = ItemId

data UnitManager = UnitManager
    { _umUnits :: Map.Map UnitId Unit
    , _umItems :: Map.Map ItemId Item
    } deriving (Show)

data UnitHealth = UnitHealth
    { _uhMax :: Int
    , _uhCurrent :: Int
    } deriving (Show)

data Condition = 
      ConditionStunned
    | ConditionDamageImmunity
    | ConditionEffectImmunity
    deriving (Show, Eq)

data Unit = Unit
    { _unitId :: UnitId
    , _unitHealth :: UnitHealth
    , _unitEquipment :: Equipment
    , _unitInventory :: Inventory
    , _unitAbilities :: Map.Map AbilitySlot Ability
    , _unitConditions :: Set.Set Condition
    } deriving (Show)

type AbilitySlot = Int
data Ability = AbilityPush
    { _abilityBaseRange :: Int
    , _abilityBaseForce :: Int
    , _abilityBaseEffect :: WorldWire UnitId ()
    } deriving (Show)

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

data Equipment = Equipment
    { equipmentSlots :: Map.Map EquipmentSlot (Maybe ItemInstance)
    } deriving (Show)

type InventoryIndex = Int
data Inventory = Inventory
    { _invItemInstances :: Map.Map InventoryIndex ItemInstance
    , _invMaxItems :: Int
    } deriving (Show)

data EquipmentSlot =
    ESHelm | ESTorso | ESHands | ESLegs | ESFoot
    | ESRing | ESPrimaryWeapon | ESSecondaryWeapon

data Item = EquipmentItem
    { _itemId :: ItemId
    } | InventoryItem
    { _itemId :: ItemId
    } deriving (Show)

data ItemInstance = ItemInstance
    { _itemInstanceId :: ItemInstanceId
    , _itemId :: ItemId
    } deriving (Show)

makeLenses ''UnitManager
makeLenses ''Unit
makeLenses ''UnitHealth
makeLenses ''Ability
makeLenses ''Equipment
makeLenses ''Inventory
makeLenses ''Item
makeLenses ''ItemInstance
