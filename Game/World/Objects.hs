{-# LANGUAGE FlexibleInstances, NamedFieldPuns, TemplateHaskell#-}
module Game.World.Objects 
	( 
	  ObjectId, ObjectIds, DoorId, DoorControllerId, SwitchId
	, PlayerId, WallId, BoulderId
	, Player(..)
	, Wall(..)
	, Door(..)
	, DoorController(..)
	, Switch(..)
	, Boulder(..)
	, ObjectPhysics(..), objectAcceleration, objectSpeed

	, tuple
	) where

import qualified Data.Set as Set
import qualified Data.Binary as B

import Linear
import Data.Monoid
import Control.Lens

type ObjectIds = Set.Set ObjectId
type ObjectId = Int

type DoorId = ObjectId
type DoorControllerId = ObjectId
type SwitchId = ObjectId
type PlayerId = ObjectId
type WallId = ObjectId
type BoulderId = ObjectId

data Wall = Wall 
	{ wallId :: WallId
	} deriving (Show, Eq)

data Player = Player
	{ playerId :: PlayerId
	, playerName :: String
	} deriving (Show, Eq)

data Door = Door
	{ doorId :: DoorId
	, doorOpen :: Bool
	} deriving (Show, Eq)

data DoorController = DoorController
	{ doorControllerId :: DoorControllerId
	, dcTargetDoorId :: DoorId
	, dcTimeRunning :: Float
	, dcTimeNeedsToOpen :: Float
	, dcStarted :: Bool
	} deriving (Show, Eq)

data Switch = Switch
	{ switchId :: SwitchId
	, switchOn :: Bool
	} deriving (Show, Eq)

data Boulder = Boulder
	{ boulderId :: BoulderId
	, boulderName :: String
	} deriving (Show, Eq)

-- No rotation for now
data ObjectPhysics = ObjectPhysics
	{ _objectAcceleration :: V2 Float
	, _objectSpeed :: V2 Float
	} deriving (Show, Eq)
makeLenses ''ObjectPhysics

tuple :: Iso' (V2 Float) (Float, Float)
tuple = iso (\(V2 x y) -> (x, y)) (uncurry V2)
 

newAccel (dx, dy) = ObjectPhysics (V2 dx dy) (V2 0 0)
instance Monoid (V2 Float) where
	mempty = V2 0 0
	mappend (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
instance Monoid ObjectPhysics where
	mempty = ObjectPhysics (V2 0 0) (V2 0 0)
	mappend (ObjectPhysics a1 v1) (ObjectPhysics a2 v2) = 
		ObjectPhysics (a1 `mappend` a2) (v1 `mappend` v2)
