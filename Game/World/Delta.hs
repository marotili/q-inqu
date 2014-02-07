{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Game.World.Delta where

import qualified Data.Map as Map
import Control.Lens
import Game.World.Objects
import Data.Monoid
import Control.Monad.Writer
import Linear

-- * Helper
alterPos2 v Nothing = Just v
alterPos2 (x1, y1) (Just (x2, y2)) = Just $ (x1 + x2, y1 + y2)

-- * Default map container
newtype (Ord k) => MapContainer k a = MapContainer { containerData :: Map.Map k a}
	deriving (Show, Eq)

instance (Monoid a, Ord k) => Monoid (MapContainer k a) where
	mempty = MapContainer Map.empty
	mappend (MapContainer m1) (MapContainer m2) = MapContainer $ Map.unionWith mappend m1 m2

-- * Object position container
newtype ObjectPositionDelta = ObjectPositionDelta
	{ deltaPos :: Map.Map ObjectId (Float, Float)
	} deriving (Show, Eq)

instance Monoid ObjectPositionDelta where
	mempty = ObjectPositionDelta Map.empty
	mappend (ObjectPositionDelta pos1) (ObjectPositionDelta pos2) = 
		ObjectPositionDelta $ foldr (\(k, v) m -> Map.alter (alterPos2 v) k m) pos1 (Map.toList pos2)

-- * World delta
data WorldDelta = WorldDelta
	{ _wdDoorsAdd :: [Door] -- absolute
	, _wdWallsAdd :: [Wall] -- absolute
	, _wdBouldersAdd :: [Boulder]
	, _wdPositionsDelta :: ObjectPositionDelta
	, _wdPhysicsDelta :: MapContainer ObjectId ObjectPhysics
	, _wdPlayerAdd :: [Player] -- absolute
	, _wdDoorControllers :: [DoorController] -- absolute
	, _wdCollisions :: MapContainer ObjectId [ObjectId]
	} deriving (Show, Eq)
makeLenses ''WorldDelta

newDelta = WorldDelta 
	{ _wdDoorsAdd = mempty
	, _wdWallsAdd = mempty
	, _wdBouldersAdd = mempty
	, _wdPositionsDelta = mempty
	, _wdPhysicsDelta = mempty
	, _wdPlayerAdd = mempty
	, _wdDoorControllers = mempty
	, _wdCollisions = mempty
	}

instance Monoid WorldDelta where
	mempty = newDelta
	mappend wd1 wd2 = newDelta
		{ _wdDoorsAdd = doors
		, _wdWallsAdd = walls
		, _wdBouldersAdd = (wd1^.wdBouldersAdd) `mappend` (wd2^.wdBouldersAdd)
		, _wdPositionsDelta = positions
		, _wdPhysicsDelta = (wd1^.wdPhysicsDelta) `mappend` (wd2^.wdPhysicsDelta)
		, _wdPlayerAdd = players
		, _wdDoorControllers = doorControllers
		, _wdCollisions = collisions
		}
		where
			walls = (wd1^.wdWallsAdd) `mappend` (wd2^.wdWallsAdd)
			doors = (wd1^.wdDoorsAdd) `mappend` (wd2^.wdDoorsAdd)
			players = (wd1^.wdPlayerAdd) `mappend` (wd2^.wdPlayerAdd)
			positions = (wd1^.wdPositionsDelta) `mappend` (wd2^.wdPositionsDelta)
			doorControllers = (wd1^.wdDoorControllers) `mappend` (wd2^.wdDoorControllers)
			collisions = (wd1^.wdCollisions) `mappend` (wd2^.wdCollisions)



-- | Write w' to w
--scribeTo :: (MonadWriter w n)
--	=> Lens' w w'
--	-> (WriterT w' Identity) () 
--	-> n ()
--scribeTo getter writer = scribe getter (execWriter writer)



-- * Writers
deltaApplyForce :: MonadWriter WorldDelta m => ObjectId -> (Float, Float) -> m ()
deltaApplyForce oId (ax, ay) = 
	scribe wdPhysicsDelta $ MapContainer $ Map.insert oId (ObjectPhysics (V2 ax ay) (V2 0 0)) Map.empty

deltaSpeed :: MonadWriter WorldDelta m => ObjectId -> (Float, Float) -> m ()	
deltaSpeed oId (vx, vy) =
	scribe wdPhysicsDelta $ MapContainer $ Map.insert oId (ObjectPhysics (V2 0 0) (V2 vx vy)) Map.empty

-- | move object using delta
deltaMoveObject :: MonadWriter WorldDelta m 
	=> ObjectId 
	-> (Float, Float) 
	-> m ()
deltaMoveObject oId dPos@(dx, dy) =
	scribe wdPositionsDelta $ ObjectPositionDelta $ Map.insert oId dPos Map.empty

-- | Add a new player to the game
deltaAddPlayer :: MonadWriter WorldDelta m 
	=> String 
	-> (Float, Float) 
	-> ObjectId 
	-> m ()
deltaAddPlayer name pos oId = do
	deltaMoveObject oId pos

	scribe wdPlayerAdd [Player oId name]

deltaAddBoulder pos name oId = deltaMoveObject oId pos >> scribe wdBouldersAdd [Boulder oId name]

-- | Add a new wall
deltaAddWall :: MonadWriter WorldDelta m => (Float, Float) -> ObjectId -> m ()
deltaAddWall pos oId = do
	-- set position
	deltaMoveObject oId pos

	scribe wdWallsAdd [Wall oId]

--deltaMoveObject' :: MonadWriter ObjectPositionDelta m
--	=> ObjectId 
--	-> (Float, Float)
--	-> m ()
--deltaMoveObject' oId dPos =
--	writer ((), )