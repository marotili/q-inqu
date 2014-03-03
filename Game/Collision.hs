{-# LANGUAGE TemplateHaskell, TypeFamilies, BangPatterns #-}
module Game.Collision where

-- * Note: we query using the maximum boundary diameter of all objects
-- *  so adding one big object may slow down the query

--import qualified Data.Octree as O
import Debug.Trace
import qualified Data.SpacePart.QuadTree as SP
import qualified Data.SpacePart.AABB as SP
import Data.Octree (Vector3(..), Octree)
import Data.Maybe
import GHC.Float
import Control.Monad
import Control.Monad.State.Strict
import Control.Lens
import Game.World.Objects
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List

-- Axis aligned box (2D for now, z ignored)
data Boundary = Boundary
	{ _boundaryOrigin :: Vector3
	, _boundarySize :: Vector3
	} deriving (Show)


newBoundary :: Boundary
newBoundary = Boundary (Vector3 0 0 0) (Vector3 0 0 0)

-- list of points that are connected by lines
data RealBoundary = RealBoundary
	{ _rbLines :: [Vector3]
	} deriving (Show)

vectorX :: Lens' Vector3 Double
vectorX = lens
	(\(Vector3 x _ _) -> x)
	(\(Vector3 _ y z) x -> Vector3 x y z)

vectorY :: Lens' Vector3 Double
vectorY = lens
	(\(Vector3 _ y _) -> y)
	(\(Vector3 x _ z) y -> Vector3 x y z)

vectorXY :: Lens' Vector3 (Double, Double)
vectorXY = lens
	(\(Vector3 x y _) -> (x, y))
	(\(Vector3 _ _ z) (x, y) -> Vector3 x y z)

toFloat :: Getter (Double, Double) (Float, Float)
toFloat = to (\(x, y) -> (double2Float x, double2Float y))

newRealBoundary :: RealBoundary
newRealBoundary = RealBoundary []

data OctreeObject = OctreeObject
	{ _ooObjectId :: ObjectId
	, _ooBoundary :: Boundary
	, _ooRealBoundary :: RealBoundary
	} deriving (Show)

newOctreeObject :: OctreeObject
newOctreeObject = OctreeObject 0 newBoundary newRealBoundary

data GameOctree = GameOctree
	{ _goMaxDiameter :: Double
	, _goStaticObjects :: Map.Map ObjectId OctreeObject
	, _goUpdatableObjects :: Map.Map ObjectId OctreeObject
	, _goStaticOctree :: SP.QuadTree OctreeObject
	, _goCachedOctree :: SP.QuadTree OctreeObject
	, _goNeedsUpdate :: Bool
	}-- deriving (Show)

makeLenses ''Boundary
makeLenses ''RealBoundary
makeLenses ''OctreeObject
makeLenses ''GameOctree

boundaries_intersect b1 b2 = 
		   ox1 + w1 > ox2
		&& ox2 + w2 > ox1
		&& oy1 + h1 > oy2
		&& oy2 + h2 > oy1
	where
		Vector3 ox1 oy1 _ = b1^.boundaryOrigin
		Vector3 w1 h1 _ = b1^.boundarySize
		Vector3 ox2 oy2 _ = b2^.boundaryOrigin
		Vector3 w2 h2 _ = b2^.boundarySize
instance SP.HasBoundary Boundary where
	--boundary_points boundary = 
	--	[ (boundary^.boundaryOrigin._1, boundary^.boundaryOrigin._2)
	--	, (boundary^.boundaryOrigin._1, boundary^.boundaryOrigin._2 + max (boundary^.boundarySize._1) (boundary^.boundarySize._2))
	--	, (boundary^.boundaryOrigin._1 + max (boundary^.boundarySize._1) (boundary^.boundarySize._2), boundary^.boundaryOrigin._2 + max (boundary^.boundarySize._1) (boundary^.boundarySize._2))
	--	, (boundary^.boundaryOrigin._1 + max (boundary^.boundarySize._1) (boundary^.boundarySize._2), boundary^.boundaryOrigin._2)
	--	]
	boundary_extents boundary = ((ox, oy)
		, (ox + max w h, oy + max w h)
		)
		where
			Vector3 ox oy _ = (boundary^.boundaryOrigin)
			Vector3 w h _ = (boundary^.boundarySize)
	boundary_square boundary = SP.Boundary (ox, oy) (max w h)
		where
			Vector3 ox oy _ = (boundary^.boundaryOrigin)
			Vector3 w h _ = (boundary^.boundarySize)

octreeObjectPoints :: Getter OctreeObject OctreeObject
octreeObjectPoints = to points
	where
		points octreeObject = octreeObject

instance SP.HasBoundary OctreeObject where
	--boundary_points obj = boundary_points (obj^.ooBoundary)
	boundary_extents obj = SP.boundary_extents (obj^.ooBoundary)
	boundary_square obj = SP.boundary_square (obj^.ooBoundary)

newOctree :: GameOctree
newOctree = GameOctree
	{ _goMaxDiameter = 0
	, _goStaticObjects = Map.empty
	, _goUpdatableObjects = Map.empty
	, _goStaticOctree = SP.empty 
	, _goCachedOctree = SP.empty
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
	mapM_ octreeAddStatic octreeObjects
	goStaticOctree %= \static -> foldr SP.insert static (octreeObjects^..traverse.octreeObjectPoints)
	static <- use goStaticOctree
	goCachedOctree .= static
	goNeedsUpdate .= True
	where
		octreeObjects = map (\(oId, origin, size) -> octreeObjectBox oId origin size) objects

		octreeAddStatic :: OctreeObject -> State GameOctree ()
		octreeAddStatic object =
			goStaticObjects %= Map.insert (object^.ooObjectId) object

octreeRemoveObject :: ObjectId -> GameOctree -> GameOctree
octreeRemoveObject oId oldOctree = oldOctree
	& goUpdatableObjects %~ Map.delete oId
	& goStaticObjects %~ Map.delete oId
	& goNeedsUpdate .~ True
	--xrn--e

octreeUpdate :: [(ObjectId, [(Float, Float)])] -> State GameOctree ()
octreeUpdate [] = return ()
octreeUpdate objects = do
	goNeedsUpdate .= True
	goUpdatableObjects %= \m -> foldr (\obj -> -- update or insert
			Map.insert (obj^.ooObjectId) obj) 
		m octreeObjects
	where
		octreeObjects :: [OctreeObject]
		octreeObjects = map (uncurry octreeObjectFromPoints) objects

octreeQueryObject :: ObjectId -> State GameOctree [ObjectId]
octreeQueryObject oId = do 
	--needUpdate <- traceShow ("needs update") $ use goNeedsUpdate
	--Control.Monad.when needUpdate $ do
	--	objs <- use goUpdatableObjects
	--	let updateObjects = map snd $ Map.toList objs
	--	staticOctree <- traceShow (updateObjects^..traverse.to (\o -> SP.boundary_extents o)) $ use goStaticOctree
	--	traceShow ("insert") $ goCachedOctree .= foldr (\oo -> traceShow "ins0" (SP.insert oo)) staticOctree (updateObjects^..traverse.octreeObjectPoints)
	--	goNeedsUpdate .= False

	queryObject' <- use goUpdatableObjects -- . at oId)
	let Just queryObject = queryObject'^.at oId
	----let points = queryObject^..octreeObjectPoints.traverse._1
	--let bound = SP.boundary_square queryObject
	--octree <- use goCachedOctree 
	--let results = traceShow ("query") $ SP.query bound octree

	--return $ filter (/= oId) $ map (^.ooObjectId) $ traceShow (results) results

	statics <- use goStaticObjects
	objs <- use goUpdatableObjects

	let intersections = filter (\obj -> obj^.ooObjectId /= oId && (obj^.ooBoundary) `boundaries_intersect` (queryObject^.ooBoundary)) 
		$ map snd $ Map.toList statics ++ Map.toList objs

	let searchBoundaryPoints = (queryObject^..ooRealBoundary.rbLines.traverse.vectorXY)

	let realIntersections = filter (\obj ->
			all (\p -> pointInConvexHull p (obj^..ooRealBoundary.rbLines.traverse.vectorXY)) searchBoundaryPoints
		) intersections

	return $ map (^.ooObjectId) realIntersections


objectLines :: [(Float, Float)] -> [((Float, Float), (Float, Float))]
objectLines points = zip points (tail points ++ [head points])

-- FIXME speed
-- for now we assume the real boundary to be convex and clockwise
pointInConvexHull :: (Double, Double) -> [(Double, Double)] -> Bool
pointInConvexHull (px, py) convexHullLines = isInside
	where
		lineSegments = zip convexHullLines (tail convexHullLines ++ [head convexHullLines])
		!normals = map (\((ax, ay), (bx, by)) -> (by - ay, -(bx - ax))) lineSegments
		!isInside = all (\((ox, oy), (x, y)) -> x*(px - ox) + y*(py - oy) > 0) $ zip convexHullLines normals


testCollision :: [ObjectId]
testCollision = evalState (do
		octreeAddStatics 
			[
			]
		octreeUpdate 
			[ (0, [(35.0, -220.0), (65.0, -190.0)])
			, (1, [(185.0, -70.0), (215.0, -40.0)])
			, (2, [(184.241058, -170.252975), (214.241058, -140.252975)])
			, (3, [(185.0, -270.0), (215.0, -240.0)])
			, (4, [(936.0, -960.0), (940.0, -956.0)])
			, (5, [(0.0, -960.0), (24.0, -24.0)])
			, (6, [(0.0, -24.0), (960.0, -48.0)])
			, (7, [(24.0, -960.0), (48.0, -48.0)])
			, (5, [(0.05, 0.95), (1, 1)]) -- does work
			, (6, [(0, 0.95), (1, 1)]) -- does not work
			]

		octreeQueryObject 2
	) newOctree
	
--_boundaryDiameter :: Boundary -> Double
----_boundaryDiameter b = O.dist (b^.boundaryOrigin) (b^.boundaryOrigin + b^.boundarySize)
--boundaryDiameter :: Getter Boundary Double
--boundaryDiameter = to _boundaryDiameter



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

		realBoundary = newRealBoundary & rbLines .~ objectBound

		objectBound = 
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
