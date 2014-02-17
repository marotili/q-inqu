{-# LANGUAGE Rank2Types, TemplateHaskell, ScopedTypeVariables #-}
module Game.World.Visibility where

import Control.Arrow
import Control.Parallel
import Control.Parallel.Strategies
import Data.Time.Clock
import Game.Collision
import Control.Lens
import Data.List
import Game.World.Import.Tiled
import Game.World.Common
import GHC.Float
import Game.World
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace
import Control.Monad.State
import Game.World.Objects


--minAngle :: (Float, Float) -> Getter OctreeObject Float
--minAngle (ox, oy) = to $ \obj -> fromJust $
--	minimumOf (traversed.vectorXY.angle (ox, oy)) (obj^.ooRealBoundary.rbLines)

--maxAngle :: (Float, Float) -> Getter OctreeObject Float
--maxAngle (ox, oy) = to $ \obj -> fromJust $
--	maximumOf (traversed.vectorXY.angle (ox, oy)) (obj^.ooRealBoundary.rbLines)

angle :: (Float, Float) -> Getter (Float, Float) Float
angle (ox, oy) = to $ \(px, py) -> acos $
	(ox - px)
	/ (sqrt ((px - ox)^2 + (py - oy)^2))

data Step = Step
	{ _stepOrigin :: (Float, Float) -- | Light origin
	, _stepObjects :: Map.Map ObjectId [(Float, Float)] -- | All objects avaiable
	, _stepActiveObjects :: Set.Set ObjectId -- | All objects that are not finished
	, _stepLine :: [(ObjectId, (Float, Float))] -- | The current sweep line

	, _stepRemainingObjects :: [(ObjectId, (Float, Float))]

	-- | info when an objects is done
	, _stepObjectNumVertices :: Map.Map ObjectId Int
	, _stepObjectNumVerticesVisited :: Map.Map ObjectId Int
	, _stepDelete :: Set.Set ObjectId
	} deriving (Show)
makeLenses ''Step

empty = Step
	{ _stepOrigin = (0, 0)
	, _stepObjects = Map.empty
	, _stepActiveObjects = Set.empty
	, _stepRemainingObjects = []
	, _stepLine = []
	, _stepObjectNumVertices = Map.empty
	, _stepObjectNumVerticesVisited = Map.empty
	, _stepDelete = Set.empty
	}

initStep :: Map.Map ObjectId [(Float, Float)] -> State Step ()
initStep objects = do
	unless (Map.size objects == 0) $ do
		origin <- use stepOrigin
		stepObjects .= objects
		stepObjectNumVertices .= Map.map length objects
		let dat = concatMap (\(oId, ps) -> map (\p -> (oId, p)) ps) $Map.toList objects
		let ordered = sortBy (sortByAngle origin) dat
		stepLine .= []
		--stepCurrentObject .= head ordered^._1
		stepLine .= [head ordered]
		stepRemainingObjects .= tail ordered
		stepObjectNumVerticesVisited . at (head ordered^._1) .= Just 1

		--currentObjId <- use stepCurrentObject
		stepActiveObjects .= Set.insert (head ordered^._1) Set.empty

	where
		sortByAngle origin obj1 obj2 = if (snd obj1^.angle origin) < (snd obj2^.angle origin)
			then LT else GT

testIntersection :: ((Float, Float), (Float, Float)) 
					-> [(Float, Float)] -> 
					[(Float, Float)]
testIntersection testLine convexPoints = catMaybes $
		(parMap rpar) (intersection testLine) (objectLines convexPoints)
	where
		objectLines :: [(Float, Float)] -> [((Float, Float), (Float, Float))]
		objectLines points = zip points (tail points ++ [head points])

intersection :: ((Float, Float), (Float, Float)) -> ((Float, Float), (Float, Float)) -> Maybe (Float, Float)
intersection ((ox, oy), (px, py)) ((ox', oy'), (px', py')) 
	| rx == rx' && ry == ry' = Nothing -- parallel
	| (rx == 0 || ry == 0) && px == px' && py == py' = Just (px', py') -- test point in origin
	| b >= 0 && b <= 1 = Just intersectionPoint
	| otherwise = Nothing
	where
		rx = px - ox
		ry = py - oy
		rx' = px' - ox'
		ry' = py' - oy'

		top = (ry/rx)*(ox' - ox) + oy - oy'
		bottom = ry' - (rx'/rx)*ry

		b = top/bottom
		a = (b*rx' + ox' - ox)/rx

		intersectionPoint = (rx*a + ox, ry*a + oy)
		intersectionPoint2 = (rx'*b + ox', ry'*b + oy')

sweep :: State Step [(ObjectId, (Float, Float))]
sweep = do
	(nextOId, point) <- fmap head $ use stepRemainingObjects
	origin <- use stepOrigin
	objects <- use stepObjects

	let testLine = (origin, point)

	stepActiveObjects %= Set.insert nextOId
	activeObjects <- use stepActiveObjects
	-- get intersections
	let dat = 
		concatMap (\(p, oId) -> map (\p -> (oId, p)) (testIntersection testLine p)) $ zip points objList
		where
			objList = (Set.toList activeObjects)
			activeObjs = map (\objId -> objects Map.! objId) objList
			points = activeObjs

 	-- update visited
	visited <- use $ stepObjectNumVerticesVisited . at nextOId
	let numVis = case visited of
		Just num -> num + 1
		Nothing -> 1
	stepObjectNumVerticesVisited . at nextOId .= Just numVis
	numVisited <- use $ stepObjectNumVerticesVisited . at nextOId
	numTotal <- use $ stepObjectNumVertices . at nextOId
	if numTotal == numVisited then
		stepActiveObjects %= Set.delete nextOId
	else
		stepActiveObjects %= Set.insert nextOId

 	-- there is an object in front of the new point
 	-- or the last point of an object -> is a corner

 	if null dat then
 		return ()
 		else do

			let minim = if numTotal == numVisited
				then do
					let others = (filter (\(oId, _) -> oId /= nextOId) dat)
					if null others then
						minimumBy (distBy origin) dat
					else
		 				minimumBy (distBy origin) others
			 	else
			 		minimumBy (distBy origin) dat

			old <- use stepLine
			let toDelete = snd $ colinear (nextOId, snd minim) old
			stepLine .= (fst $ colinear (nextOId, snd minim) old)

			stepDelete %= \oldSet -> foldr Set.insert oldSet toDelete

	stepRemainingObjects %= \old -> tail old

	return dat

 	where
 		colinear p [] = ([p], [])
 		colinear p (x:[]) = (p:x:[], [])
 		colinear (oId3, (x3, y3)) list@((oId2, (x2, y2)):(oId1, (x1, y1)):rest)
 			| rx1 == 0 && rx2 /= 0 = ((oId3, (x3, y3)):list, [])
 			| ry1 == 0 && ry2 /= 0 = ((oId3, (x3, y3)):list, [])
 			| rx1 == 0 && ry1 == 0 = (list, [])
 			| rx1 == rx2 && ry1 == ry2 = (list, [])
 			| abs (ry1*rx2 - rx1*ry2) < 0.05 = traceShow ((ry1 - rx1/rx2*ry2)) $ ((oId3, (x3, y3)):(oId1, (x1, y1)):rest, [oId2])
 			| otherwise = ((oId3, (x3, y3)):list, [])
 			where
 				kx = rx2/rx1
 				ky = ry2/ry1 
 				rx1 = (x2 - x1)
 				rx2 = (x3 - x1)
 				ry1 = (y2 - y1)
 				ry2 = (y3 - y1)
 		dist (ox, oy) (px, py) = sqrt ((px - ox)^2 + (py - oy)^2)
 		distBy origin (_, point1) (_, point2) 
 			| (dist origin point1) < (dist origin point2) = LT
 			| otherwise = GT

sweepUntil = do
	remaining <- fmap length $ use stepRemainingObjects
	if remaining == 0 then
		return ()
	else do
		sweep
		sweepUntil

test octree = do
	t <- getCurrentTime

	let octreeData = octree^.goStaticObjects
	let dataPoints = Map.fromList $
		map ((\(oId, obj) -> (oId, obj^..ooRealBoundary.rbLines.traverse.vectorXY.toFloat))) $ Map.toList octreeData

	test1 testOrigin1 dataPoints
	test1 testOrigin2 dataPoints
	test1 testOrigin3 dataPoints

	mapM_ (\o -> test1 o dataPoints) [(x, y) | x <- [500..510], y <- [400..410]]

	t2 <- getCurrentTime
	print $ diffUTCTime t2 t

	--print $ zip (
	--	orderedX^..traverse.minAngle origin
	--	) (
	--	orderedX^..traverse.maxAngle origin
	--	)
	return ()

	where
		testOrigin1 = (7*50, 21*50)
		testOrigin2 = (2*50, 3*50)
		testOrigin3 = (2*50 + 1, 3*50 + 1)

		test1 orig dataPoints = do
			let allPositive = Map.filter (\points -> 
					all (\y->y>orig^._2) $ points^..traverse._2
				) dataPoints

			let allNegative = Map.filter (\points -> 
					all (\y->y<=orig^._2) $ points^..traverse._2
				) dataPoints

			let dat = execState (do
						initStep allPositive
						sweepUntil
						return ()
					)
				(empty & stepOrigin .~ orig)

			print (dat^.stepDelete)
		 	writeFile "plot.dat" "#\tX\tY"
		 	appendFile "plot.dat"$ 
		 		concatMap (\(x, y) -> show x ++ "\t" ++ show y ++ "\n") (map snd $ dat^.stepLine)

			let dat = execState (do
						initStep allNegative
						sweepUntil
						return ()
					)
				(empty & stepOrigin .~ orig)
			print (dat^.stepDelete)
		 	appendFile "plot.dat"$ 
		 		concatMap (\(x, y) -> show x ++ "\t" ++ show y ++ "\n") (map snd $ dat^.stepLine)

mainTest = do
	tm <- tMap
	(world, manager) <- newWorldFromTiled tm

	test $ world^.wCollisionManager
