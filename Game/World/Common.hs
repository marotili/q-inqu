{-# LANGUAGE TemplateHaskell, FlexibleInstances, ExistentialQuantification, Rank2Types, TypeSynonymInstances #-}
module Game.World.Common 
( World
, WorldDelta
, ObjectProp

, wCommon
, wUnitManager
, wObjects
, wCollisionFilter
, wTileBoundary

, wcDelta

, wcPositions
, wcRotations
, wcPhysics
, wcAnimations
, wcBoundaries
, wcCollisionEvents
, wcWires
, wcOrientation
, wcStaticCollidable
, wcRealm

, wdCommon
, wdUnitManager
, wdObjects
, wdCollisionFilter

, Unit(..)
, ModifyContainer
, Modify(..)
, UnitId
, umUnits
, umdUnits
, Item
, ItemId
, umItems
, umdItems
, Rotation
, ObjectWire
, Boundary
, ObjectChangeSet

, alterPos
, wCollisionManager
, applyUnitManagerDelta

, WorldContext
, Condition(..)
, unitConditions
, Equipment(..)
, equipmentSlots
, EquipmentSlot(..)
, Inventory(..)
, invItemInstances
, invMaxItems
, UnitHealth(..)
, uhMax
, uhCurrent
, unitId
, unitHealth
, unitEquipment
, unitInventory
, unitAbilities

, WorldWire
, WireControl(..)
, wmNextObjectId

, WorldManager(..)
, wmPlayerActions

, ItemInstance(..)
, iiItemId

, emptyWM
, emptyW

, _wTileBoundary

, worldManagerUpdate
)
where

import Data.Maybe
import Game.World.Objects
import Control.Monad.RWS.Strict
import qualified Control.Wire as W
import qualified Game.Collision as C
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens
import Game.Input.Actions
import qualified Game.Input.Actions as A
import Control.Monad.State.Strict
import qualified Control.Monad as CM

type ObjectProp a = Map.Map ObjectId a
type ObjectType = Set.Set ObjectId
type Physics = Int

type Boundary = [(Float, Float)]

--type WorldContext = RWS World WorldDelta WorldManager
type DebugWorldContext = RWS World WorldDelta WorldManager
type WorldContext = DebugWorldContext

--type WorldWire a b = Wire (Timed NominalDiffTime ()) () WorldContext a b
type DebugWireS s a b = W.Wire (W.Timed W.NominalDiffTime ()) s DebugWorldContext a b
type DebugWire a b = DebugWireS () a b

data WireControl = WireRunning | WireFinished deriving (Show)
instance Monoid WireControl where
	mempty = WireFinished
	mappend WireFinished _ = WireFinished
	mappend _ WireFinished = WireFinished
	mappend _ _ = WireRunning
type ObjectWire a b = WorldWire a b

type WorldWireS s a b = DebugWireS s a b
type WorldWire a b = WorldWireS WireControl a b
type WorldSession = W.Session IO (W.Timed W.NominalDiffTime ())

data Realm = Realm
	{ _realmName :: !String
	}

--type TrackerId = ObjectId
--data TrackingManager = TrackingManager
--	{ _tmTracking :: Map.Map ObjectId [(TrackerId, ObjectWire TrackerId ())]
--	}
--newTrackingManager = TrackingManager
--	{ _tmTracking = Map.empty
--	}

type Rotation = Float
instance Monoid Float where
	mempty = 0.0
	mappend x y = x + y


data WorldCommon = WorldCommon
	{ _wcPositions :: !(ObjectProp Position)
	, _wcRotations :: !(ObjectProp Rotation)
	, _wcPhysics :: !(ObjectProp Physics)
	, _wcAnimations :: !(ObjectProp Animation)

	, _wcBoundaries :: !(ObjectProp Boundary)

	-- list of objects hit by owning object
	, _wcCollisionEvents :: !(ObjectProp [ObjectId])

	, _wcWires :: !(ObjectProp [ObjectWire ObjectId ()])
	, _wcOrientation :: !(ObjectProp Orientation)
	, _wcStaticCollidable :: !ObjectType
	, _wcRealm :: !(ObjectProp Realm)
	-- | The tracking wires are applied after the delta has been applied
	-- | worldNew = applyDelta world delta
	-- | trackDelta = runTrackWires worldNew
	-- | worldFinal = applyDelta worldNew trackDelta
	-- | renderDelta = mappend delta trackDelta
	-- we need a tree for this
	--, _wcTrack :: TrackingManager
	}

instance Show WorldCommon where
	show wc = "WorldCommon{\n" ++
		"\twcPositions = " ++ show (_wcPositions wc) ++ "\n" ++
		"\twcAnimations = " ++ show (_wcAnimations wc) ++ "\n" ++
		"\twcBoundaries = " ++ show (_wcBoundaries wc) ++ "\n" ++
		"}\n"


data WorldDelta = WorldDelta
	{ _wdCommon :: !WorldCommonDelta
	, _wdUnitManager :: !UnitManagerDelta
	, _wdObjects :: !(ObjectProp (Maybe Object)) -- add or delete objects
	, _wdCollisionFilter :: !(ObjectProp (Map.Map ObjectId (Maybe ObjectId)))
	} deriving (Show)

data World = World
    { _wCommon :: !WorldCommon
    , _wUnitManager :: !UnitManager
    , _wObjects :: !(ObjectProp Object)

	-- list of objects ignored for this object
	, _wCollisionFilter :: !(ObjectProp (Set.Set ObjectId))

    , _wCollisionManager :: !C.GameOctree
    , _wTileBoundary :: !(Float, Float)
    } deriving (Show)

data WorldManager = WorldManager
	{ _wmNextObjectId :: !ObjectId
	, _wmPlayerActions :: !(Map.Map PlayerId InputActions)
	} deriving (Show, Eq)

wcEmpty :: WorldCommon
wcEmpty = WorldCommon
 	{ _wcPositions = Map.empty
 	, _wcRotations = Map.empty
 	, _wcPhysics = Map.empty
 	, _wcAnimations = Map.empty
 	, _wcCollisionEvents = Map.empty
 	, _wcWires = Map.empty
 	, _wcBoundaries = Map.empty
 	, _wcStaticCollidable = Set.empty
 	, _wcRealm = Map.empty
 	, _wcOrientation = Map.empty
 	--, _wcTrack = newTrackingManager
 	}

emptyW :: World
emptyW = World
	{ _wCommon = wcEmpty
	, _wObjects = Map.empty
	, _wUnitManager = newUnitManager
 	, _wCollisionFilter = Map.empty
	, _wCollisionManager = C.newOctree
	, _wTileBoundary = (0, 0)
	}

emptyWM :: WorldManager
emptyWM = WorldManager
	{ _wmNextObjectId = 1
	, _wmPlayerActions = Map.empty
	}

newtype WorldCommonDelta = WorldCommonDelta
	{ _wcDelta :: WorldCommon
	} deriving (Show)
    
---
-- | Additional types

type InventoryId = Int
type ItemId = Int
type UnitId = Int
type ItemInstanceId = ItemId
-- | helper container
data Modify a = ModifySet !a | ModifyUnset deriving (Show)
type ModifyContainer k a = Map.Map k (Modify a)

data UnitManagerDelta = UnitManagerDelta
    { _umdUnits :: !(ModifyContainer UnitId Unit)
    , _umdItems :: !(ModifyContainer ItemId Item)
    } deriving (Show)

data UnitManager = UnitManager
    { _umUnits :: !(ObjectProp Unit)
    , _umItems :: !(ObjectProp Item)
    } deriving (Show)

newUnitManager :: UnitManager
newUnitManager = UnitManager
    { _umUnits = Map.empty
    , _umItems = Map.empty
    }
data UnitHealth = UnitHealth
    { _uhMax :: !Int
    , _uhCurrent :: !Int
    } deriving (Show)

data Condition = 
      ConditionStunned
    | ConditionDamageImmunity
    | ConditionEffectImmunity
    deriving (Show, Eq, Ord)

data Unit = Unit
    { _unitId :: !UnitId
    , _unitHealth :: !UnitHealth
    , _unitEquipment :: !Equipment
    , _unitInventory :: !Inventory
    , _unitAbilities :: !(Map.Map AbilitySlot Ability)
    , _unitConditions :: !(Set.Set Condition)
    } deriving (Show)

type AbilitySlot = Int
data Ability = AbilityPush
    { _abilityBaseRange :: !Int
    , _abilityBaseForce :: !Int
    , _abilityBaseEffect :: !(WorldWire UnitId ())
    }
instance Show Ability where
	show a = "Ability"

data Equipment = Equipment
    { _equipmentSlots :: !(Map.Map EquipmentSlot (Maybe ItemInstance))
    } deriving (Show)

type InventoryIndex = Int

data Inventory = Inventory
    { _invItemInstances :: !(Map.Map InventoryIndex ItemInstance)
    , _invMaxItems :: !Int
    } deriving (Show)

data EquipmentSlot =
    ESHelm | ESTorso | ESHands | ESLegs | ESFoot
    | ESRing | ESPrimaryWeapon | ESSecondaryWeapon
    deriving (Show, Eq, Ord)

data Item = EquipmentItem
    { _itemId :: !ItemId
    } | InventoryItem
    { _itemId :: !ItemId
    } deriving (Show)

data ItemInstance = ItemInstance
    { _iiItemInstanceId :: !ItemInstanceId
    , _iiItemId :: !ItemId
    } deriving (Show)

makeLenses ''UnitManager
makeLenses ''Unit
makeLenses ''UnitHealth
makeLenses ''Ability
makeLenses ''Equipment
makeLenses ''Inventory
makeLenses ''Item
makeLenses ''ItemInstance

makeLenses ''UnitManagerDelta

makeLenses ''WorldManager
makeLenses ''WorldCommonDelta
makeLenses ''WorldCommon
makeLenses ''World
makeLenses ''WorldDelta
--makeLenses ''TrackingManager	


---------------------------------------------------------
-- Delta
---------------------------------------------------------
alterMod :: Modify a -> Maybe a -> Maybe a
alterMod ModifyUnset _ = Nothing
alterMod (ModifySet v) (Just v2) = Just v 
alterMod (ModifySet v) _ = Just v

applyModification :: Ord k => ModifyContainer k a -> Map.Map k a -> Map.Map k a
applyModification modContainer mapContainer = 
    foldr (\(k, v) -> Map.alter (alterMod v) k) mapContainer $
        Map.toList modContainer

-- | apply change on world
applyUnitManagerDelta :: WorldDelta -> State World ()
applyUnitManagerDelta wd = do
    wUnitManager . umUnits %= \old -> applyModification (wd^.wdUnitManager.umdUnits) old
    wUnitManager . umItems %= \old -> applyModification (wd^.wdUnitManager.umdItems) old

-- | test
applyUnitManagerDelta' :: UnitManagerDelta -> State World ()
applyUnitManagerDelta' umd = do
    wUnitManager . umUnits %= \old -> applyModification (umd^.umdUnits) old
    wUnitManager . umItems %= \old -> applyModification (umd^.umdItems) old

-- | merge two deltas
mergeUnitManagerDelta :: UnitManagerDelta -> State UnitManagerDelta ()
mergeUnitManagerDelta umd2 = do
    umdUnits %= \old -> old `mappend` (umd2^.umdUnits)
    umdItems %= \old -> old `mappend` (umd2^.umdItems)

instance Monoid UnitManagerDelta where
    mempty = UnitManagerDelta 
        { _umdUnits = mempty
        , _umdItems = mempty
        }
    mappend um1 um2 = execState (mergeUnitManagerDelta um2) um1

alterPos :: (Float, Float) -> Maybe (Float, Float) -> Maybe (Float, Float)
alterPos val Nothing = Just val
alterPos (x, y) (Just (x', y')) = Just (x+x', y+y')

mergeCommonDelta :: WorldCommon -> State WorldCommon ()
mergeCommonDelta wc2 = do
	wcPositions %= \positions -> Map.unionWith (\(x, y) (x2, y2) -> (x + x2, y + y2)) positions (wc2^.wcPositions)
		--foldr (\(k, v) -> Map.alter (alterPos v) k) positions $
			--Map.toList (wc2^.wcPositions)

	wcRotations %= Map.unionWith mappend (wc2^.wcRotations)

	wcWires %= Map.unionWith (++) (wc2^.wcWires)
	wcAnimations %= Map.union (wc2^.wcAnimations) -- left biased
	wcBoundaries %= Map.union (wc2^.wcBoundaries)
	wcStaticCollidable %= Set.union (wc2^.wcStaticCollidable)
	wcOrientation %= Map.union (wc2^.wcOrientation)
	wcCollisionEvents %= Map.unionWith (++) (wc2^.wcCollisionEvents)

instance Monoid WorldCommonDelta where
    mempty = WorldCommonDelta wcEmpty
    mappend (WorldCommonDelta wc1) (WorldCommonDelta wc2) = 
        WorldCommonDelta (execState (mergeCommonDelta wc2) wc1)

type ObjectChangeSet = Map.Map ObjectId (Maybe ObjectId)
alterCollisionFilter :: ObjectChangeSet -> Maybe ObjectChangeSet -> Maybe ObjectChangeSet
alterCollisionFilter v Nothing = Just v

-- we can't delete and add a filter in one update
alterCollisionFilter oIds (Just oldIds) = Just (foldr (\(oId, mOId) oldIdMap ->
		case mOId of
			Just _ -> if Map.member oId oldIdMap && isNothing (oldIdMap Map.! oId)
				then error "Can't delete and add in one update"
				else Map.insert oId (Just oId) oldIdMap
			Nothing -> if Map.member oId oldIdMap && oldIdMap Map.! oId == Just oId
				then error "Can't delete and add in one update"
				else Map.insert oId (Just oId) oldIdMap
	) oldIds $ Map.toList oIds)

-- | Delta class (not used)
-- | monoid to merge two deltas and applyDelta applies the changes to the world
--class (Monoid a) => DeltaClass a where
--	applyDelta :: a -> State World ()
--	dataLens :: Lens' WorldDelta a

data DeltaClass' = forall a. DeltaClass'
	{ _applyDelta :: a -> State World ()
	, _dataLens :: Lens' WorldDelta a
	}

data Setup = Setup
	{ _deltas :: [DeltaClass']
	}

deltaSetup = Setup
	{ _deltas = 
		[ DeltaClass' 
			{ _applyDelta = applyUnitManagerDelta'
			, _dataLens = wdUnitManager
			}
		]
	}

mergeDelta :: WorldDelta -> State WorldDelta ()
mergeDelta wd2 = do
	wdCommon %= \old -> old `mappend` (wd2^.wdCommon)
	wdUnitManager %= \old -> old `mappend` (wd2^.wdUnitManager)
	wdObjects %= \old -> old `Map.union` (wd2^.wdObjects)
	wdCollisionFilter %= \oldCF ->
		foldr (\(oId, om) -> Map.alter (alterCollisionFilter om) oId) oldCF $ 
			Map.toList (wd2^.wdCollisionFilter)

-- * World delta
instance Monoid WorldDelta where
    mempty = WorldDelta 	
   		{ _wdCommon = mempty
   		, _wdUnitManager = mempty
   		, _wdObjects = Map.empty
   		, _wdCollisionFilter = Map.empty
   		}
    mappend wd1 wd2 = execState (mergeDelta wd2) wd1
        --mempty 
        --	& wdCommon .~ (wc1 `mappend` wc2) 
        --	& wdObjects .~ (obj1 `Map.union` obj2)
        --	& wdCollisionFilter .~ (wd1^.wdCollisionFilter) `mappend` (wd2^.wdCollisionFilter)

-- | test if action is spawn arrow
t a = case a of ActionSpawnArrow {} -> True; _ -> False
worldManagerUpdate :: WorldManager -> [(Int, A.Action)] -> WorldManager
worldManagerUpdate manager actions = manager2
	where
		manager0 = execState (
			mapM_ ((\pId -> do
				currentManager <- get
				let (InputActions playerActions) = if Map.member pId (currentManager^.wmPlayerActions)
					then (currentManager^.wmPlayerActions) Map.! pId
					else mempty

				wmPlayerActions %= \pa -> foldr (\action -> 
						if t action || action == ActionStopMove 
							then Map.delete pId
							else id
						) pa $ Set.toList playerActions 
				)
				. fst) (Map.toList $ manager^.wmPlayerActions)
			) manager
		manager2 = execState (
			mapM_ (\(pId, action) ->
					CM.unless (pId <= 0) $ do
						currentManager <- get
						let playerActions = if Map.member pId (currentManager^.wmPlayerActions)
							then (currentManager^.wmPlayerActions) Map.! pId
							else mempty

						wmPlayerActions %= Map.insert pId (
								playerActions `mappend` newInputAction action
							)
				) actions
			) manager0


--isCircular :: ObjectId -> TrackerId -> TrackingManager -> Bool
--isCircular oId tId tm 
--	| oId == tId = True
--	| isNothing tracking = False
--	| otherwise = any (\newTId -> isCircular oId newTId tm) $ map fst (tm^.tmTracking.at tId._Just)
--	where
--		tracking = tm^.tmTracking.at tId
