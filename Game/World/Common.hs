{-# LANGUAGE TemplateHaskell #-}
module Game.World.Common where

import Data.Maybe
import Game.World.Objects
import Control.Monad.RWS
import qualified Control.Wire as W
import qualified Game.Collision as C
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Game.Input.Actions
import qualified Game.Input.Actions as A
import Control.Monad.State
import qualified Control.Monad as CM

type ObjectProp a = Map.Map ObjectId a
type ObjectType = Set.Set ObjectId
type Position = (Float, Float)
type Physics = Int

type Boundary = [(Float, Float)]

--type WorldContext = RWS World WorldDelta WorldManager
type DebugWorldContext = RWST World WorldDelta WorldManager IO
type WorldContext = DebugWorldContext

--type WorldWire a b = Wire (Timed NominalDiffTime ()) () WorldContext a b
type DebugWireS s a b = W.Wire (W.Timed W.NominalDiffTime ()) s DebugWorldContext a b
type DebugWire a b = DebugWireS () a b

data WireControl = WireRunning | WireFinished
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
	{ _realmName :: String
	}


data WorldCommon = WorldCommon
	{ _wcPositions :: ObjectProp Position
	, _wcPhysics :: ObjectProp Physics
	, _wcAnimations :: ObjectProp Animation

	, _wcBoundaries :: ObjectProp Boundary

	-- list of objects hit by owning object
	, _wcCollisionEvents :: ObjectProp [ObjectId]

	, _wcWires :: ObjectProp [ObjectWire ObjectId ()]
	, _wcOrientation :: ObjectProp Orientation
	, _wcStaticCollidable :: ObjectType
	, _wcRealm :: ObjectProp Realm
	}

instance Show WorldCommon where
	show wc = "WorldCommon{\n" ++
		"\twcPositions = " ++ show (_wcPositions wc) ++ "\n" ++
		"\twcAnimations = " ++ show (_wcAnimations wc) ++ "\n" ++
		"}\n"


data WorldDelta = WorldDelta
	{ _wdCommon :: WorldCommonDelta
	, _wdObjects :: ObjectProp (Maybe Object) -- add or delete objects
	, _wdCollisionFilter :: ObjectProp (Map.Map ObjectId (Maybe ObjectId))
	} deriving (Show)

data World = World
    { _wCommon :: WorldCommon
    , _wObjects :: ObjectProp Object

	-- list of objects ignored for this object
	, _wCollisionFilter :: ObjectProp (Set.Set ObjectId)

    , _wCollisionManager :: C.GameOctree
    , _wTileBoundary :: (Float, Float)
    } deriving (Show)

data WorldManager = WorldManager
	{ _wmNextObjectId :: ObjectId
	, _wmPlayerActions :: Map.Map PlayerId InputActions
	} deriving (Show, Eq)

wcEmpty :: WorldCommon
wcEmpty = WorldCommon
 	{ _wcPositions = Map.empty
 	, _wcPhysics = Map.empty
 	, _wcAnimations = Map.empty
 	, _wcCollisionEvents = Map.empty
 	, _wcWires = Map.empty
 	, _wcBoundaries = Map.empty
 	, _wcStaticCollidable = Set.empty
 	, _wcRealm = Map.empty
 	, _wcOrientation = Map.empty
 	}

emptyW :: World
emptyW = World
	{ _wCommon = wcEmpty
	, _wObjects = Map.empty
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


makeLenses ''WorldManager
makeLenses ''WorldCommonDelta
makeLenses ''WorldCommon
makeLenses ''World
makeLenses ''WorldDelta

alterPos :: (Float, Float) -> Maybe (Float, Float) -> Maybe (Float, Float)
alterPos val Nothing = Just val
alterPos (x, y) (Just (x', y')) = Just (x+x', y+y')

mergeCommonDelta :: WorldCommon -> State WorldCommon ()
mergeCommonDelta wc2 = do
	wcPositions %= \positions -> Map.unionWith (\(x, y) (x2, y2) -> (x + x2, y + y2)) positions (wc2^.wcPositions)
		--foldr (\(k, v) -> Map.alter (alterPos v) k) positions $
			--Map.toList (wc2^.wcPositions)

	wcWires %= Map.unionWith (++) (wc2^.wcWires)
	wcAnimations %= Map.union (wc2^.wcAnimations) -- left biased
	wcBoundaries %= Map.union (wc2^.wcBoundaries)
	wcStaticCollidable %= Set.union (wc2^.wcStaticCollidable)
	wcOrientation %= Map.union (wc2^.wcOrientation)

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

mergeDelta :: WorldDelta -> State WorldDelta ()
mergeDelta wd2 = do
	wdCommon %= \old -> old `mappend` (wd2^.wdCommon)
	wdObjects %= \old -> old `Map.union` (wd2^.wdObjects)
	wdCollisionFilter %= \oldCF ->
		foldr (\(oId, om) -> Map.alter (alterCollisionFilter om) oId) oldCF $ 
			Map.toList (wd2^.wdCollisionFilter)

-- * World delta
instance Monoid WorldDelta where
    mempty = WorldDelta mempty Map.empty Map.empty
    mappend wd1 wd2 = execState (mergeDelta wd2) wd1
        --mempty 
        --	& wdCommon .~ (wc1 `mappend` wc2) 
        --	& wdObjects .~ (obj1 `Map.union` obj2)
        --	& wdCollisionFilter .~ (wd1^.wdCollisionFilter) `mappend` (wd2^.wdCollisionFilter)


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
						if action == ActionSpawnArrow || action == ActionStopMove 
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

