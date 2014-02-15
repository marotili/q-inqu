{-# LANGUAGE TemplateHaskell #-}
module Game.Collision where

-- * Note: we query using the maximum boundary diameter of all objects
-- *  so adding one big object may slow down the query

import qualified Data.Octree as O
import Debug.Trace
import Data.Octree (Vector3(..), Octree)
import Data.Maybe
import GHC.Float
import Control.Monad
import Control.Monad.State
import Control.Lens
import Game.World.Objects
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Axis aligned box (2D for now, z ignored)
data Boundary = Boundary
	{ _boundaryOrigin :: Vector3
	, _boundarySize :: Vector3
	} deriving (Show)
newBoundary = Boundary (Vector3 0 0 0) (Vector3 0 0 0)

-- list of points that are connected by lines
data RealBoundary = RealBoundary
	{ _rbLines :: [Vector3]
	} deriving (Show)
newRealBoundary = RealBoundary []

data OctreeObject = OctreeObject
	{ _ooObjectId :: ObjectId
	, _ooBoundary :: Boundary
	, _ooRealBoundary :: RealBoundary
	} deriving (Show)
newOctreeObject = OctreeObject 0 newBoundary newRealBoundary

data GameOctree = GameOctree
	{ _goMaxDiameter :: Double
	, _goStaticObjects :: Map.Map ObjectId OctreeObject
	, _goUpdatableObjects :: Map.Map ObjectId OctreeObject
	, _goStaticOctree :: Octree OctreeObject
	, _goCachedOctree :: Octree OctreeObject
	, _goNeedsUpdate :: Bool
	}-- deriving (Show)

makeLenses ''Boundary
makeLenses ''RealBoundary
makeLenses ''OctreeObject
makeLenses ''GameOctree

newOctree :: GameOctree
newOctree = GameOctree
	{ _goMaxDiameter = 0
	, _goStaticObjects = Map.empty
	, _goUpdatableObjects = Map.empty
	, _goStaticOctree = O.fromList []
	, _goCachedOctree = O.fromList []
	, _goNeedsUpdate = False
	}

instance Show GameOctree where
	show go = "Octree\n" ++
		"Max diameter: " ++ show (go^.goMaxDiameter) ++ "\n" ++
		"Num static: " ++ show (Map.size $ go^.goStaticObjects) ++ "\n" ++
		"Num updateable: " ++ show (Map.size $ go^.goUpdatableObjects) ++ "\n"

-- | Update octree with static data
octreeAddStatics :: [(ObjectId, (Float, Float), (Float, Float))] -> State GameOctree ()
octreeAddStatics [] = return ()
octreeAddStatics objects = do
	goMaxDiameter %= max (fromJust (octreeObjects^.maxBoundaryDiameter))
	mapM_ octreeAddStatic octreeObjects
	goStaticOctree %= \static -> foldr O.insert static (octreeObjects^.folded.octreeObjectPoints)
	static <- use goStaticOctree
	goCachedOctree .= static
	goNeedsUpdate .= True
	where
		octreeObjects = map (\(oId, origin, size) -> octreeObjectBox oId origin size) objects

		octreeAddStatic :: OctreeObject -> State GameOctree ()
		octreeAddStatic object =
			goStaticObjects %= Map.insert (object^.ooObjectId) object

octreeUpdate :: [(ObjectId, [(Float, Float)])] -> State GameOctree ()
octreeUpdate [] = return ()
octreeUpdate objects = do
	goNeedsUpdate .= True
	goUpdatableObjects %= \m -> foldr (\obj -> -- update or insert
			Map.insert (obj^.ooObjectId) obj) 
		m octreeObjects
	goMaxDiameter %= max (octreeObjects^?!maxBoundaryDiameter._Just) -- eta reduce: max new current
	where
		octreeObjects :: [OctreeObject]
		octreeObjects = map (uncurry octreeObjectFromPoints) objects

octreeQueryObject :: ObjectId -> State GameOctree [ObjectId]
octreeQueryObject oId = do 
	needUpdate <- use goNeedsUpdate
	Control.Monad.when needUpdate $ do
		objs <- use goUpdatableObjects
		let updateObjects = map snd $ Map.toList objs
		staticOctree <- use goStaticOctree
		goCachedOctree .= foldr O.insert staticOctree (updateObjects^.folded.octreeObjectPoints)
		goNeedsUpdate .= False

	Just queryObject <- use (goUpdatableObjects . at oId)
	queryRange <- use goMaxDiameter
	let points = queryObject^..octreeObjectPoints.traverse._1
	octree <- use goCachedOctree 

	-- we query using the boundary points, so the range is half the diameter
	let collisionPoints = foldr (\obj l ->
		O.withinRange octree (queryRange/2.0) obj ++ l) [] points

	let 
		otherObjects :: [OctreeObject]
		otherObjects = filter (\o -> o^.ooObjectId /= oId) $ map snd collisionPoints

		queryConvexHull :: [(Double, Double)]
		queryConvexHull = map (\(Vector3 x y z) -> (x, y)) $ queryObject^.ooRealBoundary.rbLines

		otherObjectHulls :: [[Vector3]]
		otherObjectHulls = otherObjects^..traverse.ooRealBoundary.rbLines
		otherObjectsConvexHull :: [[(Double, Double)]]
		otherObjectsConvexHull = (map.map) (\(Vector3 x y z) -> (x, y)) otherObjectHulls

		--queryObjectInOther :: Bool
		--queryObjectInOther = any (\point -> any (pointInConvexHull point) otherObjectsConvexHull) queryConvexHull
		--otherInQueryObject :: Bool
		--otherInQueryObject = any (any (`pointInConvexHull` queryConvexHull)) otherObjectsConvexHull

		-- any point of query object in hull
		queryPointInHull hull = any (\point -> pointInConvexHull point hull) queryConvexHull
		hullPointInQueryPoint hull = any (\point -> pointInConvexHull point queryConvexHull) hull

		collisions = foldr (\(obj, hull) -> if queryPointInHull hull || hullPointInQueryPoint hull
			then (++) [obj] else (++) []) [] $ zip otherObjects otherObjectsConvexHull

	return $ Set.toList . Set.fromList $ map _ooObjectId collisions

lines points = zip points (tail points ++ [head points])

-- for now we assume the real boundary to be convex and clockwise
pointInConvexHull :: (Double, Double) -> [(Double, Double)] -> Bool
pointInConvexHull (px, py) convexHullLines = isInside
	where
		lineSegments = zip convexHullLines (tail convexHullLines ++ [head convexHullLines])
		normals = map (\((ax, ay), (bx, by)) -> (by - ay, -(bx - ax))) lineSegments
		isInside = all (\((ox, oy), (x, y)) -> x*(px - ox) + y*(py - oy) > 0) $ zip convexHullLines normals


test = evalState (do
		octreeAddStatics 
			[ (0, (0, 0), (1, 1))
			, (1, (1, 0), (1, 1))
			, (2, (2, 0), (1, 1))
			, (3, (3, 0), (1, 1))
			]
		octreeUpdate 
			[ (4, [(0, 1.5), (1, 1.5)])
			, (5, [(0.05, 0.95), (1, 1.5)]) -- does work
			, (6, [(0, 0.95), (1, 1.5)]) -- does not work
			]

		octreeQueryObject 6
	) newOctree
	
_boundaryDiameter :: Boundary -> Double
_boundaryDiameter b = O.dist (b^.boundaryOrigin) (b^.boundaryOrigin + b^.boundarySize)
boundaryDiameter :: Getter Boundary Double
boundaryDiameter = to _boundaryDiameter

octreeObjectPoints :: Getter OctreeObject [(Vector3, OctreeObject)]
octreeObjectPoints = to points
	where
		points octreeObject = zip
			[ Vector3 ox oy 0
			, Vector3 ox (oy + dy) 0
			, Vector3 (ox + dx) (oy + dy) 0
			, Vector3 (ox + dx) oy 0
			] $ repeat octreeObject
			where
				Vector3 ox oy oz = octreeObject^.ooBoundary.boundaryOrigin
				Vector3 dx dy dz = octreeObject^.ooBoundary.boundarySize

-- creates a boundary that is axis aligned (the real boundary too)
-- used for tiles etc.
octreeObjectBox :: ObjectId -> (Float, Float) -> (Float, Float) -> OctreeObject
octreeObjectBox oId (ox', oy') (dx', dy') = newOctreeObject
		& ooObjectId .~ oId
		& ooBoundary .~ boundary
		& ooRealBoundary .~ realBoundary
	where
		[ox, oy, dx, dy] = map float2Double [ox', oy', dx', dy']
		boundary = newBoundary 
			& boundaryOrigin .~ Vector3 ox oy 0
			& boundarySize .~ Vector3 dx dy 0

		realBoundary = newRealBoundary & rbLines .~ lines

		lines = 
			[ Vector3 ox oy 0
			, Vector3 ox (oy + dy) 0
			, Vector3 (ox + dx) (oy + dy) 0
			, Vector3 (ox + dx) oy 0
			]

-- calculates an axis aligned boundary for the object
-- used for objects that can rotate
octreeObjectFromPoints :: ObjectId -> [(Float, Float)] -> OctreeObject
octreeObjectFromPoints oId points = newOctreeObject
		& ooObjectId .~ oId
		& ooBoundary .~ boundary
		& ooRealBoundary .~ realBoundary
	where
		xs = map (float2Double . fst) points
		ys = map (float2Double . snd) points
		(minX, maxX) = (minimum xs, maximum xs)
		(minY, maxY) = (minimum ys, maximum ys)
		boundary = newBoundary
			& boundaryOrigin .~ Vector3 minX minY 0
			& boundarySize .~ Vector3 (maxX - minX) (maxY - minY) 0

		-- Note: xs and ys were converted to doubles (we can't use points)
		realBoundary = newRealBoundary & rbLines
			.~ map (\(x, y) -> Vector3 x y 0) (zip xs ys)

maxBoundaryDiameter :: Getter [OctreeObject] (Maybe Double)
maxBoundaryDiameter = to $ 
	maximumOf (traversed.ooBoundary.boundaryDiameter) -- octreeObjects