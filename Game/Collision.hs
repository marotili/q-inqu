{-# LANGUAGE TemplateHaskell, NamedFieldPuns #-}
module Game.Collision 
	( 
	) where

import GHC.Float
import Debug.Trace
import Data.SpacePart.QuadTree
import Data.SpacePart.AABB

import Game.World.Objects (ObjectId)

import Control.Monad.State
import Control.Monad

import Control.Lens

import qualified Data.Set as Set
import qualified Data.Map as Map


--type ObjectId = Int
emptyBoundary = Boundary (0, 0) 0
newBoundary :: (Float, Float) -> Float -> Boundary
newBoundary (dx, dy) ds = Boundary (float2Double dx, float2Double dy) (float2Double ds)

data CollidableObject = CollidableObject
	{ objectId :: ObjectId
	, _objectBoundary :: Boundary
	} deriving (Show)
makeLenses ''CollidableObject

newCollidable :: ObjectId -> Boundary -> CollidableObject
newCollidable = CollidableObject

instance Eq CollidableObject where
	(==) co1 co2 = objectId co1 == objectId co2

instance Ord CollidableObject where
	compare co1 co2 = compare (objectId co1) (objectId co2)

data CollisionManager = CollisionManager
	{ _cmStaticObjects :: Map.Map ObjectId CollidableObject
	, _cmFloatingObjects :: Map.Map ObjectId CollidableObject
	, _cmStaticQuadTree :: QuadTree CollidableObject -- without floating
	, _cmCachedQuadTree :: QuadTree CollidableObject -- with floating
	, _cmNeedsUpdate :: Bool
	}
makeLenses ''CollisionManager

instance Eq CollisionManager where
	(==) cm1 cm2 = (cm1^.cmStaticObjects == cm2^.cmStaticObjects) &&
		(cm1^.cmFloatingObjects == cm2^.cmFloatingObjects)

cmNew :: CollisionManager
cmNew = CollisionManager 
	{ _cmStaticObjects = Map.empty
	, _cmFloatingObjects = Map.empty
	, _cmStaticQuadTree = empty
	, _cmCachedQuadTree = empty
	, _cmNeedsUpdate = False
	}

cmAddStatic :: CollidableObject -> State CollisionManager ()
cmAddStatic object = do
	cmStaticObjects %= Map.insert (objectId object) object
	cmStaticQuadTree %= insert object

cmAddFloating :: CollidableObject -> State CollisionManager ()
cmAddFloating object = do
	cmFloatingObjects %= Map.insert (objectId object) object
	cmNeedsUpdate .= True

cmRemoveFloating :: ObjectId -> State CollisionManager ()
cmRemoveFloating objectId = do
	--let tmpObject = CollidableObject objectId emptyBoundary
	cmFloatingObjects %= Map.delete objectId
	cmNeedsUpdate .= True

cmUpdateQT :: State CollisionManager ()
cmUpdateQT = do
	cm <- get
	let update = cm^.cmNeedsUpdate
	let floatObjects = cm^.cmFloatingObjects
	let staticQT = cm^.cmStaticQuadTree
	traceShow "stateless" $
		when update $ do
			cmCachedQuadTree .= staticQT
			traceShow "insert statics" $
				cmCachedQuadTree %= \qt -> 
					foldr (insert.snd) qt (Map.toList floatObjects)
			traceShow "insert floats" $
				cmNeedsUpdate .= False
	traceShow "updated" (return ())

cmQuery :: Boundary -> State CollisionManager [ObjectId]
cmQuery b = do
	traceShow "cmUpdateQt" cmUpdateQT
	cm <- get

	let qt = traceShow "cached" $ cm^.cmCachedQuadTree
	
	let results = traceShow ("Test" ++ show (cm^.cmFloatingObjects))  $
		query b qt
	traceShow ("results" ++ show (Set.toList . Set.fromList $ map objectId results)) $
		return $ Set.toList . Set.fromList $ map objectId results

cmObjectBoundarySize oId = do
	cm <- get
	return $ if Map.member oId (cm ^. cmStaticObjects)
		then double2Float (boundary cm cmStaticObjects)
		else 
			if Map.member oId (cm ^. cmFloatingObjects)
				then double2Float (boundary cm cmFloatingObjects)
				else 0
	where
		boundary cm objectSource = boundary_size ((cm ^. objectSource) Map.! oId ^. objectBoundary)	

cmObjectPos :: ObjectId -> State CollisionManager (Float, Float)
cmObjectPos oId = do
	cm <- get
	return $ if Map.member oId (cm ^. cmStaticObjects)
		then conv (pos cm cmStaticObjects)
		else 
			if Map.member oId (cm ^. cmFloatingObjects)
				then conv (pos cm cmFloatingObjects)
				else (0, 0)

	where
		conv (dx, dy) = (double2Float dx, double2Float dy)
		pos cm objectSource = boundary_corner ((cm ^. objectSource) Map.! oId ^. objectBoundary)	

cmUpdateFloating :: ObjectId -> (Float, Float) -> State CollisionManager ()
cmUpdateFloating oId (x, y) = do
	boundarySize <- cmObjectBoundarySize oId
	cmRemoveFloating oId
	cmAddFloating $ CollidableObject oId (newBoundary (x, y) boundarySize)

cmCollisions :: ObjectId -> State CollisionManager [ObjectId]
cmCollisions oId = do
	cm <- get
	let floatingObjects = cm^.cmFloatingObjects
	let boundary = filter (\obj -> oId == objectId obj) (map snd (Map.toList floatingObjects))
	if null boundary
		then return []
		else do
			let b = head boundary ^. objectBoundary
			collisions <- cmQuery b
			return $ filter (/= oId) collisions

cmCanCollide :: ObjectId -> CollisionManager -> Bool
cmCanCollide oId cm = Map.member oId (cm^.cmStaticObjects) || 
		Map.member oId (cm^.cmFloatingObjects)

instance HasBoundary CollidableObject where
	boundary_edges (CollidableObject { _objectBoundary }) = boundary_edges _objectBoundary
	boundary_extents (CollidableObject { _objectBoundary }) = boundary_extents _objectBoundary
	boundary_square (CollidableObject { _objectBoundary }) = boundary_square _objectBoundary
