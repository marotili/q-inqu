{-# LANGUAGE FlexibleContexts, TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module Game.World.Delta where

import qualified Data.Map as Map
import Control.Lens
import Game.World.Objects
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.RWS
import Linear
--import Game.World.Types
import Game.Collision
import Control.Monad.State
import Control.Monad
import qualified Control.Wire as W
import Data.Maybe
import Game.World.Common

--import qualified Prelude as P

-- * Helper
alterPos2 v Nothing = Just v
alterPos2 (x1, y1) (Just (x2, y2)) = Just (x1 + x2, y1 + y2)

---- * Default map container
--newtype (Ord k) => MapContainer k a = MapContainer 
--	{ containerData :: Map.Map k a
--	} deriving (Show, Eq)

--instance (Monoid a, Ord k) => Monoid (MapContainer k a) where
--	mempty = MapContainer Map.empty
--	mappend (MapContainer m1) (MapContainer m2) = MapContainer $ Map.unionWith mappend m1 m2

---- * Object position container
--newtype ObjectPositionDelta = ObjectPositionDelta
--	{ deltaPos :: Map.Map ObjectId (Float, Float)
--	} deriving (Show, Eq)

--instance Monoid ObjectPositionDelta where
--	mempty = ObjectPositionDelta Map.empty
--	mappend (ObjectPositionDelta pos1) (ObjectPositionDelta pos2) = 
--		ObjectPositionDelta $ foldr (\(k, v) m -> Map.alter (alterPos2 v) k m) pos1 (Map.toList pos2)


--data WorldWire a b = WorldWire a b deriving (Show)


instance Monoid WorldCommonDelta where
	mempty = WorldCommonDelta wcEmpty
	mappend (WorldCommonDelta d1) (WorldCommonDelta d2) = 
		WorldCommonDelta d2

-- * World delta
instance Monoid WorldDelta where
	mempty = WorldDelta mempty Map.empty
	mappend (WorldDelta wc1 obj1) (WorldDelta wc2 obj2) = 
		WorldDelta wc2 (obj1 `Map.union` obj2)


objectExists :: ObjectId -> World -> Bool
objectExists oId w = Map.member oId (w^.wObjects)
wObjectExists :: ObjectId -> Getter World Bool
wObjectExists oId = to (objectExists oId)

type Get a = Getter World a
type Set a = Setter' WorldDelta a

data Component a da = Component
	{ _compGet :: Getter World a
	, _compSet :: Setter' WorldDelta da
	}
--makeLenses ''Component (ObjectProp Position) (ObjectProp Position)
--compGet = to _compGet
--compSet = to _compSet

type Component' a = Component a a

compObject :: Component (ObjectProp Object) (ObjectProp (Maybe Object))
compObject = Component
	{ _compGet = wObjects
	, _compSet = wdObjects
	}

compPosition :: Component' (ObjectProp Position)
compPosition = Component 
	{ _compGet = wCommon.wcPositions
	, _compSet = wdCommon.delta.wcPositions
	}	

compWires :: Component' (ObjectProp [ObjectWire])
compWires = Component
	{ _compGet = wCommon.wcWires
	, _compSet = wdCommon.delta.wcWires
	}

compAnimations :: Component' (ObjectProp Animation)
compAnimations = Component
	{ _compGet = wCommon.wcAnimations
	, _compSet = wdCommon.delta.wcAnimations
	}

getWires :: Get (ObjectProp [ObjectWire])
getWires = _compGet compWires
setWires :: Set (ObjectProp [ObjectWire])
setWires = _compSet compWires

writeProp :: (MonadWriter WorldDelta m) 
	=> Set (ObjectProp a)
	-> ObjectId 
	-> a 
	-> m ()
writeProp set oId a = scribe (set . at oId) (Just a)

addWire = writeProp setWires

setAnimations :: Set (ObjectProp Animation)
setAnimations = _compSet compAnimations
getAnimations :: Get (ObjectProp Animation)
getAnimations = _compGet compAnimations

setAnimation :: (MonadWriter WorldDelta m) => ObjectId -> Animation -> m ()
setAnimation oId anim = writeProp setAnimations oId anim

setPositions :: Setter' WorldDelta (ObjectProp Position)
setPositions = _compSet compPosition
getPositions :: Getter World (ObjectProp Position)
getPositions = _compGet compPosition

moveObject oId dPos = writeProp setPositions oId dPos

setObjects :: Setter' WorldDelta (ObjectProp (Maybe Object))
setObjects = _compSet compObject
getObjects :: Getter World (ObjectProp Object)
getObjects = _compGet compObject

addObject :: (MonadWriter WorldDelta m) => ObjectId -> Object -> m ()
addObject oId obj = writeProp setObjects oId (Just obj)
deleteObject :: (MonadWriter WorldDelta m) => ObjectId -> m ()
deleteObject oId = writeProp setObjects oId Nothing

--object :: ObjectId -> WorldWire a (Maybe Object)
--objectI :: ObjectId -> WorldWire a (Maybe Object)

test :: WriterT WorldDelta IO ()
test = 
	return () 

runTest = runWriterT test

--instance Monoid WorldDelta where
--	mempty = newDelta
--	mappend wd1 wd2 = newDelta
--		{ _wdDoorsAdd = doors
--		, _wdObjectsAdd = (wd1^.wdObjectsAdd) `mappend` (wd2^.wdObjectsAdd)
--		, _wdWallsAdd = walls
--		, _wdBouldersAdd = (wd1^.wdBouldersAdd) `mappend` (wd2^.wdBouldersAdd)
--		, _wdPositionsDelta = positions
--		, _wdPhysicsDelta = (wd1^.wdPhysicsDelta) `mappend` (wd2^.wdPhysicsDelta)
--		, _wdPlayerAdd = players
--		, _wdDoorControllers = doorControllers
--		, _wdCollisions = collisions
--		, _wdAnimations = Map.union (wd2^.wdAnimations) (wd1^.wdAnimations) -- left-biased
--		, _wdCollisionCallbacks = Map.union (wd2^.wdCollisionCallbacks) (wd1^.wdCollisionCallbacks) -- left-biased
--		, _wdCollisionEvents = (wd1^.wdCollisionEvents) `mappend` (wd2^.wdCollisionEvents)
--		}
--		where
--			walls = (wd1^.wdWallsAdd) `mappend` (wd2^.wdWallsAdd)
--			doors = (wd1^.wdDoorsAdd) `mappend` (wd2^.wdDoorsAdd)
--			players = (wd1^.wdPlayerAdd) `mappend` (wd2^.wdPlayerAdd)
--			positions = (wd1^.wdPositionsDelta) `mappend` (wd2^.wdPositionsDelta)
--			doorControllers = (wd1^.wdDoorControllers) `mappend` (wd2^.wdDoorControllers)
--			collisions = (wd1^.wdCollisions) `mappend` (wd2^.wdCollisions)



-- | Write w' to w
--scribeTo :: (MonadWriter w n)
--	=> Lens' w w'
--	-> (WriterT w' Identity) () 
--	-> n ()
--scribeTo getter writer = scribe getter (execWriter writer)

--deltaAnim :: MonadWriter WorldDelta m => ObjectId -> Animation -> m ()
--deltaAnim oId anim = scribe wdAnimations $ Map.insert oId anim Map.empty

---- * Writers
--deltaApplyForce :: MonadWriter WorldDelta m => ObjectId -> (Float, Float) -> m ()
--deltaApplyForce oId (ax, ay) = 
--	scribe wdPhysicsDelta $ MapContainer $ Map.insert oId (ObjectPhysics (V2 ax ay) (V2 0 0)) Map.empty

--deltaSpeed :: MonadWriter WorldDelta m => ObjectId -> (Float, Float) -> m ()	
--deltaSpeed oId (vx, vy) =
--	scribe wdPhysicsDelta $ MapContainer $ Map.insert oId (ObjectPhysics (V2 0 0) (V2 vx vy)) Map.empty

---- | move object using delta
--deltaMoveObject :: (MonadWriter WorldDelta m)
--	=> ObjectId -> (Float, Float) 
--	-> m ()
--deltaMoveObject oId dPos@(dx, dy) = 
--	scribe wdPositionsDelta $ ObjectPositionDelta $ Map.insert oId dPos Map.empty


---- | Add a new player to the game
--deltaAddPlayer :: MonadWriter WorldDelta m 
--	=> String 
--	-> (Float, Float) 
--	-> ObjectId 
--	-> m ()
--deltaAddPlayer name pos oId = do
--	deltaMoveObject oId pos

--	scribe wdPlayerAdd [Player oId name]

--deltaAddBoulder pos name oId = deltaMoveObject oId pos >> scribe wdBouldersAdd [Boulder oId name]

--deltaSetCollisionCb :: MonadWriter WorldDelta m => CollisionCallback -> ObjectId -> m ()
--deltaSetCollisionCb cb oId = 
--	scribe wdCollisionCallbacks $ Map.insert oId cb Map.empty

--deltaCollisionEvent :: MonadWriter WorldDelta m => ObjectId -> ObjectId -> m ()
--deltaCollisionEvent oId1 oId2 =
--	scribe wdCollisionEvents [(oId1, oId2)]

---- | Add a new wall
--deltaAddWall :: MonadWriter WorldDelta m => (Float, Float) -> ObjectId -> m ()
--deltaAddWall pos oId = do
--	-- set position
--	deltaMoveObject oId pos

--	scribe wdWallsAdd [Wall oId]

--deltaAddObject :: MonadWriter WorldDelta m => String -> (Float, Float) -> ObjectId -> m ()
--deltaAddObject name pos oId = do
--	deltaMoveObject oId pos
--	scribe wdObjectsAdd [Object oId name]

----deltaMoveObject' :: MonadWriter ObjectPositionDelta m
----	=> ObjectId 
----	-> (Float, Float)
----	-> m ()
----deltaMoveObject' oId dPos =
----	writer ((), )