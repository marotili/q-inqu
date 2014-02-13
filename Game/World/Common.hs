{-# LANGUAGE TemplateHaskell #-}
module Game.World.Common where

import Game.World.Objects
import Control.Monad.RWS
import qualified Control.Wire as W
import Game.Collision
import qualified Data.Map as Map
import Control.Lens
import Game.Input.Actions
import Control.Monad.State
import Data.Monoid

type ObjectProp a = Map.Map ObjectId a
type Position = (Float, Float)
type Physics = Int

data WorldCommon = WorldCommon
	{ _wcPositions :: ObjectProp Position
	, _wcPhysics :: ObjectProp Physics
	, _wcAnimations :: ObjectProp Animation
	, _wcCollisions :: ObjectProp [ObjectId]
	, _wcWires :: ObjectProp [ObjectWire]
	}

instance Show WorldCommon where
	show wc = "WorldCommon{\n" ++
		"wcPositions = " ++ show (_wcPositions wc) ++ "\n" ++
		"}\n"

--type WorldContext = RWS World WorldDelta WorldManager
type DebugWorldContext = RWST World WorldDelta WorldManager IO
type WorldContext = DebugWorldContext

--type WorldWire a b = Wire (Timed NominalDiffTime ()) () WorldContext a b
type DebugWire a b = W.Wire (W.Timed W.NominalDiffTime ()) () DebugWorldContext a b
type WorldWire a b = DebugWire a b 
type WorldSession = W.Session IO (W.Timed W.NominalDiffTime ())

data WorldDelta = WorldDelta
	{ _wdCommon :: WorldCommonDelta
	, _wdObjects :: ObjectProp (Maybe Object) -- add or delete objects
	} deriving (Show)

data World = World
    { _wCommon :: WorldCommon
    , _wObjects :: ObjectProp Object
    , _wCollisionManager :: CollisionManager
    , _wTileBoundary :: (Float, Float)
    } deriving (Show)

data WorldManager = WorldManager
	{ _wmNextObjectId :: ObjectId
	, _wmPlayerActions :: Map.Map PlayerId InputActions
	} deriving (Show, Eq)

wcEmpty = WorldCommon
 	{ _wcPositions = Map.empty
 	, _wcPhysics = Map.empty
 	, _wcAnimations = Map.empty
 	, _wcCollisions = Map.empty
 	, _wcWires = Map.empty
 	}

emptyW = World
	{ _wCommon = wcEmpty
	, _wObjects = Map.empty
	, _wCollisionManager = cmNew
	, _wTileBoundary = (0, 0)
	}

emptyWM = WorldManager
	{ _wmNextObjectId = 1
	, _wmPlayerActions = Map.empty
	}

newtype WorldCommonDelta = WorldCommonDelta
	{ _delta :: WorldCommon
	} deriving (Show)

type ObjectWire = W.Wire (W.Timed W.NominalDiffTime ()) () WorldContext ObjectId ()

makeLenses ''WorldManager
makeLenses ''WorldCommonDelta
makeLenses ''WorldCommon
makeLenses ''World
makeLenses ''WorldDelta

alterPos val Nothing = Just val
alterPos (x, y) (Just (x', y')) = Just $ (x+x', y+y')

mergeCommonDelta :: WorldCommon -> State WorldCommon ()
mergeCommonDelta wc2 = do
	wcPositions %= \positions ->
		foldr (\(k, v) -> Map.alter (alterPos v) k) positions $
			Map.toList (wc2^.wcPositions)

instance Monoid WorldCommonDelta where
    mempty = WorldCommonDelta wcEmpty
    mappend (WorldCommonDelta wc1) (WorldCommonDelta wc2) = 
        WorldCommonDelta (execState (mergeCommonDelta wc2) wc1)

-- * World delta
instance Monoid WorldDelta where
    mempty = WorldDelta mempty Map.empty
    mappend (WorldDelta wc1 obj1) (WorldDelta wc2 obj2) = 
        WorldDelta (wc1 `mappend` wc2) (obj1 `Map.union` obj2)

