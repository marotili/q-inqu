{-# LANGUAGE FlexibleContexts, TemplateHaskell, Rank2Types, NoMonomorphismRestriction #-}
module Game.World.Delta where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Debug.Trace
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
import Game.World.Lens

--applyCommonDelta :: State World ()
applyCommonDelta wd = do
	-- add delta to last position or insert new
	wCommon.wcPositions %= \positions ->
		foldr (\(k, v) -> Map.alter (alterPos v) k) positions $ 
			Map.toList (wd^.wdCommon.delta.wcPositions)

	-- take the latest animations
	wCommon.wcAnimations %= \animations -> 
		(wd^.wdCommon.delta.wcAnimations) `Map.union` animations -- left biased

	-- we drop all wires from the last state
	wCommon.wcWires .= wd^.wdCommon.delta.wcWires

	wCommon.wcOrientation %= \old -> Map.union (wd^.wdCommon.delta.wcOrientation) old

	-- take latest boundaries
	wCommon.wcBoundaries %= \old -> Map.union (wd^.wdCommon.delta.wcBoundaries) old

	wCommon.wcStaticCollidable %= \old -> Set.union (wd^.wdCommon.delta.wcStaticCollidable) old

	world <- get
	let newStaticCollidables = Set.toList $ wd^.wdCommon.delta.wcStaticCollidable
	let newStatic = map (\oId -> 
			let (pos, size) = world^.tileBoundary oId in (oId, pos, size)
		) newStaticCollidables

	unless (null newStatic) $
		wCollisionManager %= execState (octreeAddStatics newStatic)

	world2 <- get
	-- pos and boundary for collision
	let 
		objectsWithPos = Set.fromList $ world2^..wCommon.wcPositions.itraversed.asIndex
		objectsWithBoundary = Set.fromList $ world2^..wCommon.wcBoundaries.itraversed.asIndex	

		objPosAndBoundary = Set.intersection objectsWithPos objectsWithBoundary

		objNewBoundaries = Set.fromList $ (wd^..wdCommon.delta.wcBoundaries.itraversed.asIndex)
		objNewPos = Set.fromList $ (wd^..wdCommon.delta.wcPositions.itraversed.asIndex)

		objUpdate = Set.toList $ Set.intersection objPosAndBoundary $ objNewBoundaries `Set.union` objNewPos 

		objBoundaries = zip objUpdate [world2^.objectBoundary oId | oId <- objUpdate]

	unless (null objBoundaries) $
		wCollisionManager %= execState (octreeUpdate objBoundaries)

alterObjects delta@Nothing _ = Nothing -- delte object
alterObjects (Just v) _ = Just v -- add / update object

applyObjectDelta wd = do
	wObjects %= \objects -> 
		foldr (\(k, v) -> Map.alter (alterObjects v) k) objects $
			Map.toList (wd^.wdObjects)

--alterColFilter Nothing _ -> Nothing
--alterColFilter 

applyCollisionFilterDelta wd = do
	wCollisionFilter %= \filterMap -> foldr (\(oId, objMap) oldIds ->
		Map.insert oId (
			foldr (\(objId, mObjId) objSet -> case mObjId of
				Just _ -> Set.insert objId objSet
				Nothing -> Set.delete objId objSet
			) (if Map.member oId oldIds then oldIds Map.! oId else Set.empty) $ 
				Map.toList objMap
			) oldIds
		) filterMap $ Map.toList (wd^.wdCollisionFilter)

applyDelta :: World -> WorldDelta -> World
applyDelta w wd = execState (do
		applyObjectDelta wd
		applyCollisionFilterDelta wd
		applyCommonDelta wd
	) w
