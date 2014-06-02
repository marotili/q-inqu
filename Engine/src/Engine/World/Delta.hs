{-# LANGUAGE FlexibleContexts, Rank2Types, NoMonomorphismRestriction, BangPatterns #-}
module Engine.World.Delta where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Lens
import Control.Monad.Writer
import Control.Monad.RWS
--import Engine.World.Types
import Engine.Collision
import Control.Monad.State.Strict
import qualified Control.Monad.State as LS
import Engine.World.Common
import Engine.World.Lens
import Data.Maybe
import Data.List

applyCommonDelta :: WorldDelta -> State World ()
applyCommonDelta wd = do
	-- add delta to last position or insert new
	wCommon.wcPositions %= \positions ->
		foldr (\(k, v) -> Map.alter (alterPos v) k) positions $ 
			Map.toList (wd^.wdCommon.wcDelta.wcPositions)

	-- take the latest animations
	wCommon.wcAnimations %= \animations -> 
		(wd^.wdCommon.wcDelta.wcAnimations) `Map.union` animations -- left biased

	wCommon.wcRotations %= \old -> Map.unionWith mappend old (wd^.wdCommon.wcDelta.wcRotations)


	-- we drop all wires from the last state
	wCommon.wcWires .= wd^.wdCommon.wcDelta.wcWires
	-- we drop all events from last state
	wCommon.wcCollisionEvents .= (wd^.wdCommon.wcDelta.wcCollisionEvents)

	wCommon.wcOrientation %= Map.union (wd^.wdCommon.wcDelta.wcOrientation)

	-- take latest boundaries
	wCommon.wcBoundaries %= Map.union (wd^.wdCommon.wcDelta.wcBoundaries)

	wCommon.wcStaticCollidable %= Set.union (wd^.wdCommon.wcDelta.wcStaticCollidable)

	world <- get
	let newStaticCollidables = Set.toList $ wd^.wdCommon.wcDelta.wcStaticCollidable
	--let newStatic = map (\oId -> 
	--		let (pos, size) = world^.tileBoundary oId in (oId, pos, size)
	--	) newStaticCollidables

	--unless (null newStatic) $
	--	wCollisionManager %= execState (octreeAddStatics newStatic)

	world2 <- get
	-- pos and boundary for collision
	let 
		objectsWithPos = Set.fromList $ world2^..wCommon.wcPositions.itraversed.asIndex
		objectsWithBoundary = Set.fromList $ world2^..wCommon.wcBoundaries.itraversed.asIndex	

		objPosAndBoundary = Set.intersection objectsWithPos objectsWithBoundary

		objNewBoundaries = Set.fromList (wd^..wdCommon.wcDelta.wcBoundaries.itraversed.asIndex)
		objNewPos = Set.fromList (wd^..wdCommon.wcDelta.wcPositions.itraversed.asIndex)

		objUpdate = Set.toList $ Set.intersection objPosAndBoundary $ objNewBoundaries `Set.union` objNewPos 

		!objBoundaries = zip objUpdate [world2^.objectBoundary oId | oId <- objUpdate]

	unless (null objBoundaries) $
		wCollisionManager %= execState (octreeUpdate objBoundaries)

alterObjects :: Maybe a -> Maybe a -> Maybe a
alterObjects Nothing _ = Nothing -- delte object
alterObjects (Just v) _ = Just v -- add / update object

applyObjectDelta :: WorldDelta -> State World ()
applyObjectDelta wd = do
	let objectsToDelete = map fst . filter (isNothing . snd) $ Map.toList (wd^.wdObjects)
	wCommon.wcPositions %= \old -> foldr Map.delete old objectsToDelete
	wCommon.wcRotations %= \old -> foldr Map.delete old objectsToDelete
	wCommon.wcPhysics %= \old -> foldr Map.delete old objectsToDelete
	wCommon.wcAnimations %= \old -> foldr Map.delete old objectsToDelete
	wCommon.wcBoundaries %= \old -> foldr Map.delete old objectsToDelete
	wCommon.wcCollisionEvents %= \old -> foldr Map.delete old objectsToDelete
	wCommon.wcWires %= \old -> foldr Map.delete old objectsToDelete
	wCommon.wcOrientation %= \old -> foldr Map.delete old objectsToDelete
	wCommon.wcStaticCollidable %= \old -> foldl' (flip Set.delete) old objectsToDelete
	wCommon.wcRealm %= \old -> foldr Map.delete old objectsToDelete

	wUnitManager.umUnits %= \old -> foldr Map.delete old objectsToDelete
	wUnitManager.umItems %= \old -> foldr Map.delete old objectsToDelete
	wCollisionFilter %= \old -> foldr Map.delete old objectsToDelete
	wCollisionManager %= \old -> foldr octreeRemoveObject old objectsToDelete

	wObjects %= \objects -> 
		foldr (\(k, v) -> Map.alter (alterObjects v) k) objects $
			Map.toList (wd^.wdObjects)
--alterColFilter Nothing _ -> Nothing
--alterColFilter 

applyCollisionFilterDelta :: WorldDelta -> State World ()
applyCollisionFilterDelta wd =
	wCollisionFilter %= \filterMap -> foldr (\(oId, objMap) oldIds ->
		Map.insert oId (
			foldr (\(objId', mObjId) objSet -> case mObjId of
				Just _ -> Set.insert objId' objSet
				Nothing -> Set.delete objId' objSet
			) (if Map.member oId oldIds then oldIds Map.! oId else Set.empty) $ 
				Map.toList objMap
			) oldIds
		) filterMap $ Map.toList (wd^.wdCollisionFilter)

applyDelta :: World -> WorldDelta -> World
applyDelta w wd = execState (do
		applyObjectDelta wd
		applyCollisionFilterDelta wd
		applyCommonDelta wd
		applyUnitManagerDelta wd
	) w
