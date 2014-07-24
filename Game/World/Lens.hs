{-# LANGUAGE FlexibleContexts, Rank2Types, NoMonomorphismRestriction #-}
module Game.World.Lens 
    ( objectBoundary
    , Get
    , Set
    , Component(..)
    , writeProp
    , compUnit

    , getWires
    , addWire
    , addObject
    , moveObject
    , isCollidable
    , getCollisionFilters
    , setCollisionEvent
    , getAnimations
    , setAnimation
    , setAnimations
    , deleteObject

    , objectPosition

    , rotateObject
    , getPositions
    , collisionEvent
    , setIgnoreCollision
    , setBoundary
    , getItems
    , getObjects
    , setOrientation
    , setStaticCollidable
    )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Lens
import Game.World.Objects
import Control.Monad.Writer
--import Game.World.Types
import Data.Maybe
import Game.World.Common
import Control.Arrow

--objectExists :: ObjectId -> World -> Bool
--objectExists oId w = Map.member oId (w^.wObjects)
--wObjectExists :: ObjectId -> Getter World Bool
--wObjectExists oId = to (objectExists oId)

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

compUnit :: Component (ObjectProp Unit) (ModifyContainer UnitId Unit)
compUnit = Component
    { _compGet = wUnitManager.umUnits
    , _compSet = wdUnitManager.umdUnits
    }

compItem :: Component (ObjectProp Item) (ModifyContainer ItemId Item)
compItem = Component
    { _compGet = wUnitManager.umItems
    , _compSet = wdUnitManager.umdItems
    }

compPosition :: Component' (ObjectProp Position)
compPosition = Component 
    { _compGet = wCommon.wcPositions
    , _compSet = wdCommon.wcDelta.wcPositions
    }    

compRotation :: Component' (ObjectProp Rotation)
compRotation = Component
    { _compGet = wCommon.wcRotations
    , _compSet = wdCommon.wcDelta.wcRotations
    }

compWires :: Component' (ObjectProp [ObjectWire ObjectId ()])
compWires = Component
    { _compGet = wCommon.wcWires
    , _compSet = wdCommon.wcDelta.wcWires
    }

compAnimations :: Component' (ObjectProp Animation)
compAnimations = Component
    { _compGet = wCommon.wcAnimations
    , _compSet = wdCommon.wcDelta.wcAnimations
    }

compBoundaries :: Component' (ObjectProp Boundary)
compBoundaries = Component
    { _compGet = wCommon.wcBoundaries
    , _compSet = wdCommon.wcDelta.wcBoundaries
    }

compOrientation :: Component' (ObjectProp Orientation)
compOrientation = Component
    { _compGet = wCommon.wcOrientation
    , _compSet = wdCommon.wcDelta.wcOrientation
    }

compCollisionEvent :: Component' (ObjectProp [ObjectId])
compCollisionEvent = Component
    { _compGet = wCommon.wcCollisionEvents
    , _compSet = wdCommon.wcDelta.wcCollisionEvents
    }

type IngoredObjects = Set.Set ObjectId
type ObjectIdTo a = ObjectProp a
type ListOfChanges = Map.Map ObjectId (Maybe ObjectId)
type CollisionFilter = Component
--type ObjectChangeSet = Map.Map ObjectId (Maybe ObjectId)

compCollisionFilter :: CollisionFilter (ObjectIdTo IngoredObjects) (ObjectIdTo ObjectChangeSet)
compCollisionFilter = Component
    { _compGet = wCollisionFilter
    , _compSet = wdCollisionFilter
    }

getWires :: Get (ObjectProp [ObjectWire ObjectId ()])
getWires = _compGet compWires
setWires :: Set (ObjectProp [ObjectWire ObjectId ()])
setWires = _compSet compWires

writeProp :: (MonadWriter WorldDelta m) 
    => Set (ObjectProp a)
    -> ObjectId 
    -> a 
    -> m ()
writeProp mapSetter oId a = scribe (mapSetter . at oId) (Just a)

addWire :: (MonadWriter WorldDelta m) => ObjectId -> ObjectWire ObjectId () -> m ()
addWire oId w = writeProp setWires oId [w]

setAnimations :: Set (ObjectProp Animation)
setAnimations = _compSet compAnimations
getAnimations :: Get (ObjectProp Animation)
getAnimations = _compGet compAnimations

setAnimation :: (MonadWriter WorldDelta m) => ObjectId -> Animation -> m ()
setAnimation = writeProp setAnimations

setPositions :: Setter' WorldDelta (ObjectProp Position)
setPositions = _compSet compPosition
getPositions :: Getter World (ObjectProp Position)
getPositions = _compGet compPosition

objectPosition :: ObjectId -> Getter World (Maybe Position)
objectPosition oId = getPositions . at oId

-- | Rotation component
setRotations :: Setter' WorldDelta (ObjectProp Rotation)
setRotations = _compSet compRotation
getRotations :: Getter World (ObjectProp Rotation)
getRotations = _compGet compRotation

rotateObject :: (MonadWriter WorldDelta m) 
    => ObjectId -> Rotation -> m ()
rotateObject = writeProp setRotations 
objectRotation :: ObjectId -> Get (Maybe Rotation)
objectRotation oId = getRotations . at oId

moveObject :: (MonadWriter WorldDelta m) => ObjectId -> (Float, Float) -> m ()
moveObject = writeProp setPositions

setObjects :: Setter' WorldDelta (ObjectProp (Maybe Object))
setObjects = _compSet compObject
getObjects :: Getter World (ObjectProp Object)
getObjects = _compGet compObject

addObject :: (MonadWriter WorldDelta m) => ObjectId -> Object -> m ()
addObject oId obj = writeProp setObjects oId (Just obj)
deleteObject :: (MonadWriter WorldDelta m) => ObjectId -> m ()
deleteObject oId = writeProp setObjects oId Nothing

deletedObjects :: Getter WorldDelta [ObjectId]
deletedObjects = to getDeleted
    where
        getDeleted wd = getDeletedObjects (wd^.wdObjects)
        -- new objects are inserted into the map with Just
        getDeletedObjects objectMap = map fst $
            filter (\(objId, mobj) -> case mobj of Nothing -> True; _ -> False) $
            Map.toList objectMap

newObjects :: Getter WorldDelta [Object]
newObjects = to getNew
    where
        getNew wd = getNewObjects (wd^.wdObjects)
        -- new objects are inserted into the map with Just
        getNewObjects objectMap = map fromJust . 
            filter (\mobj -> case mobj of Just _ -> True; _ -> False) $
            map snd $ Map.toList objectMap

findObject :: String -> Getter World (Maybe Object)
findObject name = to (\w -> 
        unOne $ ifoldMap (\_ obj -> One $
            if (obj^.objName) == name then Just obj else Nothing
        ) (w^.wObjects)
    )

-- | boundary + position
tileBoundary :: ObjectId -> Get ((Float, Float), (Float, Float))
tileBoundary oId = to boundary
    where 
        boundary w = (pos w, w^.wTileBoundary)
        pos w = fromJust $ w^.getPositions . at oId

-- | boundary + position
objectBoundary :: ObjectId -> Get Boundary
objectBoundary oId = to boundary
    where
        boundary w = let (px, py) = pos w in  -- collision boundary = object boundary + position
            map ((+) px *** (+) py) $ fromJust $ w^.getBoundaries . at oId
        pos w = fromJust $ w^.getPositions . at oId

setBoundary :: (MonadWriter WorldDelta m) => ObjectId -> Boundary -> m ()
setBoundary = writeProp setBoundaries

setBoundaries :: Set (ObjectProp Boundary)
setBoundaries = _compSet compBoundaries
getBoundaries :: Get (ObjectProp Boundary)
getBoundaries = _compGet compBoundaries

setStaticCollidable :: (MonadWriter WorldDelta m) => ObjectId -> m ()
setStaticCollidable oId = scribe (wdCommon.wcDelta.wcStaticCollidable) (Set.insert oId Set.empty)

isCollidable :: ObjectId -> Get Bool
isCollidable oId = to collidable
    where
        collidable w = Set.member oId $ objPosAndBoundary w
        objectsWithPos w = Set.fromList $ w^..wCommon.wcPositions.itraversed.asIndex
        objectsWithBoundary w = Set.fromList $ w^..wCommon.wcBoundaries.itraversed.asIndex  

        objPosAndBoundary w = Set.intersection (objectsWithPos w) (objectsWithBoundary w)

setOrientations :: Set (ObjectProp Orientation)
setOrientations = _compSet compOrientation
getOrientations :: Get (ObjectProp Orientation)
getOrientations = _compGet compOrientation

setOrientation :: (MonadWriter WorldDelta m) => ObjectId -> Orientation -> m ()
setOrientation = writeProp setOrientations

getCollisionFilters :: Get (ObjectProp (Set.Set ObjectId))
getCollisionFilters = _compGet compCollisionFilter
setCollisionFilters :: Set (ObjectProp (Map.Map ObjectId (Maybe ObjectId)))
setCollisionFilters = _compSet compCollisionFilter

setIgnoreCollision :: (MonadWriter WorldDelta m) => ObjectId -> ObjectId -> m ()
setIgnoreCollision oId otherId = writeProp setCollisionFilters oId (Map.fromList [(otherId, Just otherId)])
unsetIgnoreCollision :: (MonadWriter WorldDelta m) => ObjectId -> ObjectId -> m ()
unsetIgnoreCollision oId otherId = writeProp setCollisionFilters oId (Map.fromList [(otherId, Nothing)])

setCollisionEvents :: Set (ObjectProp [ObjectId])
setCollisionEvents = _compSet compCollisionEvent
getCollisionEvents :: Get (ObjectProp [ObjectId])
getCollisionEvents = _compGet compCollisionEvent

setCollisionEvent :: (MonadWriter WorldDelta m) => ObjectId -> ObjectId -> m ()
setCollisionEvent oId otherId = writeProp setCollisionEvents oId [otherId]

collisionEvent :: ObjectId -> Get [ObjectId]
collisionEvent oId = to (\w -> fromMaybe [] $ w^.getCollisionEvents . at oId)

--collided :: ObjectId -> ObjectId -> Get Bool
--collided oId otherId = to (\w -> otherId `elem` (w^.collisionEvent oId))

setItems :: Set (ModifyContainer ItemId Item)
setItems = _compSet compItem
getItems :: Get (ObjectProp Item)
getItems = _compGet compItem

--unitsInDistance :: ObjectId -> Float -> Get Set.Set ObjectId
--unitsInDistance oId distance = to get
--    where
--        get world = let
--            Just oPos = world^.wcPositions.at oId
