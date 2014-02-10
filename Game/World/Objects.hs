{-# LANGUAGE FlexibleInstances, NamedFieldPuns, TemplateHaskell#-}
module Game.World.Objects 
	( 
	  ObjectId, ObjectIds, DoorId, DoorControllerId, SwitchId
	, PlayerId, WallId, BoulderId
	, Object(..), objId, objName
	, Player(..)
	, Wall(..)
	, Door(..)
	, DoorController(..)
	, Switch(..)
	, Boulder(..)
	, ObjectPhysics(..), objectAcceleration, objectSpeed
	, Animation(..), animTime, animCurrentTime, animNext, animTileGid
	, defaultCharacterAnim, beeAnim, dinoAnim, arrowAnim
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


data Object = Object
	{ _objId :: ObjectId
	, _objName :: String
	} deriving (Show, Eq)
makeLenses ''Object

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

data Animation = Animation
    { _animId :: Int -- local id for now TODO
    , _animTileGid :: Int
    , _animTime :: Float
    , _animNext :: Animation
    , _animCurrentTime :: Float
    }
makeLenses ''Animation

instance Show Animation where
	show anim = show (anim^.animTileGid) ++ " / " ++ show (anim^.animTime) ++ " / " ++ show (anim ^.animCurrentTime)

instance Eq Animation where
	a1 == a2 = (a1^.animId) == (a2^.animId)

arrowAnim = a1
	where
		a1 = Animation 200 113 4 a1 0
defaultCharacterAnim :: (Float, Float) -> Animation
defaultCharacterAnim (dx, dy) 
	| dy > 0.5 = a1 
	| dy < -0.5 = b1
	| dx > 0.5 = c1
	| dx < -0.5 = d1
	| otherwise = a1
    where
        a1 = Animation 1 73 0.25 a2 0 
        a2 = Animation 1 74 0.25 a3 0
        a3 = Animation 1 75 0.25 a4 0
        a4 = Animation 1 76 0.25 a1 0

        b1 = Animation 2 81 0.25 b2 0
        b2 = Animation 2 82 0.25 b3 0
        b3 = Animation 2 83 0.25 b4 0
        b4 = Animation 2 84 0.25 b1 0

        c1 = Animation 3 89 0.25 c2 0
        c2 = Animation 3 90 0.25 c3 0
        c3 = Animation 3 91 0.25 c4 0
        c4 = Animation 3 92 0.25 c1 0

        d1 = Animation 4 101 0.25 d2 0
        d2 = Animation 4 102 0.25 d3 0
        d3 = Animation 4 103 0.25 d4 0
        d4 = Animation 4 104 0.25 d1 0

dinoAnim = a1
    where
        a1 = Animation 5 105 2 a2 0 
        a2 = Animation 5 106 0.1 a3 0
        a3 = Animation 5 107 0.1 a4 0
        a4 = Animation 5 108 0.1 a1 0

beeAnim = a1
    where
        a1 = Animation 6 109 2 a2 0 
        a2 = Animation 6 110 0.1 a3 0
        a3 = Animation 6 111 0.1 a4 0
        a4 = Animation 6 112 0.1 a1 0