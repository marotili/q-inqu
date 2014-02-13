{-# LANGUAGE TemplateHaskell #-}
module Game.World.Common where

import Game.World.Objects
import Control.Monad.RWS
import qualified Control.Wire as W
import Game.Collision
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Game.Input.Actions
import Control.Monad.State
import Data.Monoid
import qualified Control.Monad as CM

type ObjectProp a = Map.Map ObjectId a
type Position = (Float, Float)
type Physics = Int

data WorldCommon = WorldCommon
	{ _wcPositions :: ObjectProp Position
	, _wcPhysics :: ObjectProp Physics
	, _wcAnimations :: ObjectProp Animation
	, _wcCollisions :: ObjectProp [ObjectId]
	, _wcWires :: ObjectProp [ObjectWire ObjectId ()]
	}

instance Show WorldCommon where
	show wc = "WorldCommon{\n" ++
		"wcPositions = " ++ show (_wcPositions wc) ++ "\n" ++
		"}\n"

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

	wcWires %= Map.unionWith (++) (wc2^.wcWires)

instance Monoid WorldCommonDelta where
    mempty = WorldCommonDelta wcEmpty
    mappend (WorldCommonDelta wc1) (WorldCommonDelta wc2) = 
        WorldCommonDelta (execState (mergeCommonDelta wc2) wc1)

-- * World delta
instance Monoid WorldDelta where
    mempty = WorldDelta mempty Map.empty
    mappend (WorldDelta wc1 obj1) (WorldDelta wc2 obj2) = 
        WorldDelta (wc1 `mappend` wc2) (obj1 `Map.union` obj2)

worldManagerUpdate manager actions = manager2
	where
		manager0 = execState (do
			mapM_ (\pId -> do
				manager <- get
				let (InputActions playerActions) = if Map.member pId (manager^.wmPlayerActions)
					then (manager^.wmPlayerActions) Map.! pId
					else mempty

				wmPlayerActions %= \pa -> foldr (\action -> 
						if action == ActionSpawnArrow || action == ActionStopMove 
							then Map.delete pId
							else id
						) pa $ Set.toList playerActions 
				) (map fst $ Map.toList $ manager^.wmPlayerActions)
			) manager
		manager2 = execState (do
			mapM_ (\(pId, action) -> do
					CM.unless (pId <= 0) $ do
						manager <- get
						let playerActions = if Map.member pId (manager^.wmPlayerActions)
							then (manager^.wmPlayerActions) Map.! pId
							else mempty

						wmPlayerActions %= Map.insert pId (
								playerActions `mappend` newInputAction action
							)
				) actions
			) manager0

