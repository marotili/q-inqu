{-# LANGUAGE FlexibleContexts, TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module Game.World.Lens where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Game.World.Objects
import Control.Monad.Writer
import Control.Monad.RWS
import Linear
--import Game.World.Types
import qualified Game.Collision as C
import Control.Monad.State
import Control.Monad
import qualified Control.Wire as W
import Data.Maybe
import Game.World.Common

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

compPosition :: Component' (ObjectProp Position)
compPosition = Component 
    { _compGet = wCommon.wcPositions
    , _compSet = wdCommon.delta.wcPositions
    }    

compWires :: Component' (ObjectProp [ObjectWire ObjectId ()])
compWires = Component
    { _compGet = wCommon.wcWires
    , _compSet = wdCommon.delta.wcWires
    }

compAnimations :: Component' (ObjectProp Animation)
compAnimations = Component
    { _compGet = wCommon.wcAnimations
    , _compSet = wdCommon.delta.wcAnimations
    }

compBoundaries :: Component' (ObjectProp Boundary)
compBoundaries = Component
    { _compGet = wCommon.wcBoundaries
    , _compSet = wdCommon.delta.wcBoundaries
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
writeProp set oId a = scribe (set . at oId) (Just a)

addWire oId w = writeProp setWires oId [w]

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

objectPosition :: ObjectId -> Getter World (Maybe Position)
objectPosition oId = getPositions . at oId

moveObject oId dPos = writeProp setPositions oId dPos

setObjects :: Setter' WorldDelta (ObjectProp (Maybe Object))
setObjects = _compSet compObject
getObjects :: Getter World (ObjectProp Object)
getObjects = _compGet compObject

addObject :: (MonadWriter WorldDelta m) => ObjectId -> Object -> m ()
addObject oId obj = writeProp setObjects oId (Just obj)
deleteObject :: (MonadWriter WorldDelta m) => ObjectId -> m ()
deleteObject oId = writeProp setObjects oId Nothing

newObjects :: Getter WorldDelta [Object]
newObjects = to getNew
    where
        getNew wd = newObjects (wd^.wdObjects)
        -- new objects are inserted into the map with Just
        newObjects objectMap = map fromJust . 
            filter (\mobj -> case mobj of Just obj -> True; _ -> False) $
            map snd $ Map.toList objectMap

newtype One a = One { unOne :: Maybe a }
instance Monoid (One a) where
    mempty = One Nothing
    mappend (One Nothing) (One x) = One x
    mappend (One x) (One Nothing) = One x

findObject :: String -> Getter World (Maybe Object)
findObject name = to (\w -> 
        unOne $ ifoldMap (\oId obj -> 
            if (obj^.objName) == name then (One (Just obj)) else One Nothing
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
            map (\(x, y) ->  (px + x, py + y)) $ fromJust $ w^.getBoundaries . at oId
        pos w = fromJust $ w^.getPositions . at oId

setBoundary :: (MonadWriter WorldDelta m) => ObjectId -> Boundary -> m ()
setBoundary oId b = writeProp setBoundaries oId b

setBoundaries :: Set (ObjectProp Boundary)
setBoundaries = _compSet compBoundaries
getBoundaries :: Get (ObjectProp Boundary)
getBoundaries = _compGet compBoundaries

setStaticCollidable :: (MonadWriter WorldDelta m) => ObjectId -> m ()
setStaticCollidable oId = scribe (wdCommon.delta.wcStaticCollidable) (Set.insert oId Set.empty)

isCollidable :: ObjectId -> Get Bool
isCollidable oId = to collidable
    where
        collidable w = Set.member oId $ objPosAndBoundary w
        objectsWithPos w = Set.fromList $ w^..wCommon.wcPositions.itraversed.asIndex
        objectsWithBoundary w = Set.fromList $ w^..wCommon.wcBoundaries.itraversed.asIndex  

        objPosAndBoundary w = Set.intersection (objectsWithPos w) (objectsWithBoundary w)
        
--object :: ObjectId -> WorldWire a (Maybe Object)
--objectI :: ObjectId -> WorldWire a (Maybe Object)newtype One = One { unOne :: Maybe ObjectId }




--test :: Getter World [(ObjectId, String)]
--test = to (\w -> itoList (w^.wName))

-- Get id by name
-- get name by id

--objectIdByName :: String -> Getter World (Maybe ObjectId)
--objectIdByName name = to (\w -> 
--        unOne $ ifoldMap (\oId name' -> 
--            if name' == name then (One (Just oId)) else One Nothing
--        ) (w^.wName)
--    )