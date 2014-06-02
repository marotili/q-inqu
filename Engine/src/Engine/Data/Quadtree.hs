{-# LANGUAGE TemplateHaskell #-}
module Engine.Data.Quadtree where

import Control.Lens
import Debug.Trace
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe

-- | insert axis aligned rectangles
-- | query lines and axis aligned rectangles

type NodeId = Int
type NodeDataId = Int
type ObjectId = Int

data Quadrant = 
	  NWQuad 
	| NEQuad 
	| SEQuad 
	| SWQuad
	deriving (Show)
-- | axis aligned bounding box
data Boundary = Boundary
	{ _boundaryTopLeft :: (Float, Float)
	, _boundarySize :: (Float, Float)
	} deriving (Show)
newBoundary topLeft size = Boundary
	{ _boundaryTopLeft = topLeft
	, _boundarySize = size
	}

data Size = Infty | Size Float deriving (Show)
data NodeSize = NodeSize Size Size deriving (Show)

data NodeData = NodeData
	{ _ndId :: NodeDataId
	, _ndObjectId :: ObjectId
	, _ndBoundary :: Boundary
	, _ndNodes :: Set.Set NodeId
	} deriving (Show)

data QuadtreeNode = QuadtreeNode
	{ _qtNodeId :: NodeId
	, _qtNodeQuadrant :: Quadrant
	, _qtNodeParentId :: Maybe NodeId
	, _qtNodeSize :: NodeSize
	, _qtNodeChilds :: Maybe ( Float, Float -- center
							 , NodeId, NodeId
							 , NodeId, NodeId
							 )
	--, _qtNodeCenter :: Maybe (Float, Float) -- 
	, _qtNodeData :: Set.Set NodeDataId
	, _qtProxy :: Bool
	} deriving (Show)

data Quadtree = Quadtree
	{ _qtNodes :: Map.Map NodeId QuadtreeNode
	, _qtNodeDatas :: Map.Map NodeDataId NodeData
	, _qtNextId :: Int
	} deriving (Show)

makeLenses ''Boundary
makeLenses ''NodeData
makeLenses ''QuadtreeNode
makeLenses ''Quadtree

qtNodeCenter = to (\qtNode -> 
		qtNode^.qtNodeChilds >>= \nodeChilds -> Just (nodeChilds^._1, nodeChilds^._2)
	)

qtNodeChildNW :: Lens QuadtreeNode QuadtreeNode (Maybe NodeId) (Maybe NodeId)
qtNodeChildNW = lens (\qtNode -> qtNode^.qtNodeChilds >>= \node -> Just $ node^._3) 
	(\qtNode (Just qtNW) -> qtNode & qtNodeChilds._Just._3 .~ qtNW)
qtNodeChildNE = to (\qtNode -> qtNode^.qtNodeChilds >>= \node -> Just $ node^._4)
qtNodeChildSW = to (\qtNode -> qtNode^.qtNodeChilds >>= \node -> Just $ node^._5)
qtNodeChildSE = to (\qtNode -> qtNode^.qtNodeChilds >>= \node -> Just $ node^._6)


boundaryBottom = to (\boundary ->
		boundary^.boundaryTopLeft._2 + boundary^.boundarySize._2
	)

boundaryTop = to (\boundary ->
		boundary^.boundaryTopLeft._2
	)

boundaryRight = to (\boundary ->
		boundary^.boundaryTopLeft._1 + boundary^.boundarySize._1
	)

boundaryLeft = to (\boundary ->
		boundary^.boundaryTopLeft._1
	)

boundaryBottomRight = to (\boundary ->
		( boundary^.boundaryTopLeft._1 + boundary^.boundarySize._1
		, boundary^.boundaryTopLeft._2 + boundary^.boundarySize._2
		)
	)

boundaryTopRight = to (\boundary ->
		( boundary^.boundaryTopLeft._1 + boundary^.boundarySize._1
		, boundary^.boundaryTopLeft._2
		)
	)

boundaryBottomLeft = to (\boundary ->
		( boundary^.boundaryTopLeft._1
		, boundary^.boundaryTopLeft._2 + boundary^.boundarySize._2
		)
	)

allNW boundary center@(x, y) = bx < x && by < y
	where
		(bx, by) = boundary^.boundaryBottomRight

allNE boundary center@(x, y) = bx >= x && by < y
	where
		(bx, by) = boundary^.boundaryBottomLeft

allSW boundary center@(x, y) = bx >= x && by >= y
	where
		(bx, by) = boundary^.boundaryTopRight

allSE boundary center@(x, y) = bx < x && by >= y
	where
		(bx, by) = boundary^.boundaryTopLeft

allS boundary center@(_, y) = by >= y
	where
		by = boundary^.boundaryTop

allN boundary center@(_, y) = by < y
	where
		by = boundary^.boundaryBottom

allW boundary center@(x, _) = bx < x
	where
		bx = boundary^.boundaryRight

allE boundary center@(x, _) = bx >= x
	where
		bx = boundary^.boundaryLeft

emptyNode nId = QuadtreeNode
	{ _qtNodeId = nId
	, _qtNodeQuadrant = NWQuad -- arbitrary
	, _qtNodeParentId = Nothing -- 0 is root
	, _qtNodeSize = NodeSize Infty Infty
	, _qtNodeChilds = Nothing 
	--, _qtNodeCenter = Just (0, 0)
	, _qtNodeData = Set.empty
	, _qtProxy = False
	}

emptyQuadtree = Quadtree
	{ _qtNodes = Map.insert 1 (emptyNode 1) Map.empty
	, _qtNodeDatas = Map.empty
	, _qtNextId = 2 -- root node has nodeId 1
	}

qtNewId = do
	newId <- use qtNextId
	qtNextId += 1
	return newId

newChildDimensions qtNode boundary = traceShow ("child dimensions") $ do
	case (qtNode^.qtNodeParentId) of
		Just parentId -> do
			Just parent <- use $ qtNodes . at parentId
			let Just (px, py) = parent^.qtNodeCenter
			let parentSize = parent^.qtNodeSize
			case parentSize of
				-- root node
				NodeSize Infty Infty -> case (qtNode^.qtNodeQuadrant) of
					NWQuad -> do
						let (bx, by) = boundary^.boundaryTopLeft
						return (bx, by
							, NodeSize Infty Infty, NodeSize (Size $ abs (bx - px)) Infty
							, NodeSize Infty (Size (by - py)), NodeSize (Size $ abs (bx - px)) (Size $ by - py)
							)
					NEQuad -> do
						let (bx, by) = boundary^.boundaryTopRight
						return (bx, by
							, NodeSize (Size $ bx - px) Infty, NodeSize Infty Infty
							, NodeSize (Size $ bx - px) (Size $ by - py), NodeSize Infty (Size $ by - py)
							)
					SWQuad -> do
						let (bx, by) = boundary^.boundaryBottomRight
						return (bx, by
							, NodeSize Infty (Size $ abs (by - py)), NodeSize (Size $ abs (bx - px)) (Size $ abs (by - py))
							, NodeSize Infty Infty, NodeSize (Size $ abs (bx - px)) Infty
							)
					SEQuad -> do
						let (bx, by) = boundary^.boundaryTopLeft
						return (bx, by
							, NodeSize (Size $ abs (bx - px)) (Size $ abs (by - py)), NodeSize Infty (Size $ abs (by - py))
							, NodeSize (Size $ abs (bx - px)) Infty, NodeSize Infty Infty
							)
				NodeSize Infty (Size h) -> do
					case (qtNode^.qtNodeQuadrant) of
						NWQuad -> do
							let bx = boundary^.boundaryLeft
							return (bx, py - h/2.0
								, NodeSize Infty (Size $ h/2.0), NodeSize (Size $ abs (bx - px)) (Size $ h/2.0)
								, NodeSize Infty (Size $ h/2.0), NodeSize (Size $ abs (bx - px)) (Size $ h/2.0)
								)
						SWQuad -> do
							let bx = boundary^.boundaryLeft
							return (bx, py + h/2.0
								, NodeSize Infty (Size $ h/2.0), NodeSize (Size $ abs (bx - px)) (Size $ h/2.0)
								, NodeSize Infty (Size $ h/2.0), NodeSize (Size $ abs (bx - px)) (Size $ h/2.0)
								)
						NEQuad -> do
							let bx = boundary^.boundaryRight
							return (bx, py - h/2.0
								, NodeSize (Size $ abs (bx - px)) (Size $ h/2.0), NodeSize Infty (Size $ h/2.0)
								, NodeSize (Size $ abs (bx - px)) (Size $ h/2.0), NodeSize Infty (Size $ h/2.0)
								)
						SEQuad -> do
							let bx = boundary^.boundaryRight
							return (bx, py + h/2.0
								, NodeSize (Size $ abs (bx - px)) (Size $ h/2.0), NodeSize Infty (Size $ h/2.0)
								, NodeSize (Size $ abs (bx - px)) (Size $ h/2.0), NodeSize Infty (Size $ h/2.0)
								)
				NodeSize (Size w) Infty -> do
					case (qtNode^.qtNodeQuadrant) of
						NWQuad -> do
							let by = boundary^.boundaryTop
							return (px - w / 2.0, by
								, NodeSize (Size $ w/2.0) Infty, NodeSize (Size $ w/2.0) Infty
								, NodeSize (Size $ w/2.0) (Size $ abs (by - py)), NodeSize (Size $ w/2.0) (Size $ abs (by - py)) 
								)
						NEQuad -> do
							let by = boundary^.boundaryTop
							return (px + w / 2.0, by
								, NodeSize (Size $ w/2.0) Infty, NodeSize (Size $ w/2.0) Infty
								, NodeSize (Size $ w/2.0) (Size $ abs (by - py)), NodeSize (Size $ w/2.0) (Size $ abs (by - py)) 
								)

						SWQuad -> do
							let by = boundary^.boundaryBottom
							return (px - w/2.0, by
								, NodeSize (Size $ w/2.0) (Size $ abs (by - py)), NodeSize (Size $ w/2.0) Infty
								, NodeSize (Size $ w/2.0) (Size $ abs (by - py)), NodeSize (Size $ w/2.0) Infty
								)
						SEQuad -> do
							let by = boundary^.boundaryBottom
							return (px + w/2.0, by
								, NodeSize (Size $ w/2.0) (Size $ abs (by - py)), NodeSize (Size $ w/2.0) Infty
								, NodeSize (Size $ w/2.0) (Size $ abs (by - py)), NodeSize (Size $ w/2.0) Infty
								)

				NodeSize (Size w) (Size h) -> do
					case (qtNode^.qtNodeQuadrant) of
						NWQuad -> do
							return (px - w/2.0, py - h/2.0
								, NodeSize (Size $ w/2.0) (Size $ h/2.0), NodeSize (Size $ w/2.0) (Size $ h/2.0)
								, NodeSize (Size $ w/2.0) (Size $ h/2.0), NodeSize (Size $ w/2.0) (Size $ h/2.0)
								)
						NEQuad -> do
							return (px + w/2.0, py - h/2.0
								, NodeSize (Size $ w/2.0) (Size $ h/2.0), NodeSize (Size $ w/2.0) (Size $ h/2.0)
								, NodeSize (Size $ w/2.0) (Size $ h/2.0), NodeSize (Size $ w/2.0) (Size $ h/2.0)
								)
						SWQuad -> do
							return (px - w/2.0, py + h/2.0
								, NodeSize (Size $ w/2.0) (Size $ h/2.0), NodeSize (Size $ w/2.0) (Size $ h/2.0)
								, NodeSize (Size $ w/2.0) (Size $ h/2.0), NodeSize (Size $ w/2.0) (Size $ h/2.0)
								)
						SEQuad -> do
							return (px + w/2.0, py + h/2.0
								, NodeSize (Size $ w/2.0) (Size $ h/2.0), NodeSize (Size $ w/2.0) (Size $ h/2.0)
								, NodeSize (Size $ w/2.0) (Size $ h/2.0), NodeSize (Size $ w/2.0) (Size $ h/2.0)
								)

		Nothing -> do -- root node
			return (0, 0, NodeSize Infty Infty, NodeSize Infty Infty, NodeSize Infty Infty, NodeSize Infty Infty)


newChildNode :: NodeId -> QuadtreeNode -> Quadrant -> NodeSize -> QuadtreeNode
newChildNode nId parentNode quadrant size = emptyNode nId
	& qtNodeId .~ nId
	& qtNodeQuadrant .~ quadrant 
	& qtNodeParentId .~ (Just $ parentNode^.qtNodeId)
	& qtNodeSize .~ size

qtNodeInsert nodeData qtNode
	| qtNode^.qtProxy = traceShow ("qtNodeInsert proxy") $ do
		Just qtNW <- use $ qtNodes . at (fromJust $ qtNode^.qtNodeChildNW)
		Just qtNE <- use $ qtNodes . at (fromJust $ qtNode^.qtNodeChildNE)
		Just qtSW <- use $ qtNodes . at (fromJust $ qtNode^.qtNodeChildSW)
		Just qtSE <- use $ qtNodes . at (fromJust $ qtNode^.qtNodeChildSE)
		qtNodeInsert nodeData qtNW 
		qtNodeInsert nodeData qtNE
		qtNodeInsert nodeData qtSW 
		qtNodeInsert nodeData qtSE

	| otherwise = traceShow ("qtNodeInsert otherwise") $ do
		case (qtNode^.qtNodeParentId) of
			Just parentId -> do
				Just parent <- use $ qtNodes . at parentId
				let Just (px, py) = parent^.qtNodeCenter
				let NodeSize w h = qtNode^.qtNodeSize

				let topLeft w h = case (qtNode^.qtNodeQuadrant) of
					NWQuad -> (px - w, py - h)
					NEQuad -> (px, py - h)
					SWQuad -> (px - w, py)
					SEQuad -> (px, py)

				let bottomLeft w h = case (qtNode^.qtNodeQuadrant) of
					NWQuad -> (px, py)
					NEQuad -> (px + w, py)
					SWQuad -> (px, py + h)
					SEQuad -> (px + w, py + h)

				let (bx0, by0) = (nodeData^.ndBoundary.boundaryTopLeft)
				let (bx1, by1) = (nodeData^.ndBoundary.boundaryBottomRight)

				let cond = case (w, h) of	
					(Infty, Infty) -> case (qtNode^.qtNodeQuadrant) of
						NWQuad -> px >= bx0 && py >= by0
						NEQuad -> px < bx1 && py >= by0
						SWQuad -> px >= bx0 && py < by1
						SEQuad -> px < bx1 && py < by1
					(Infty, Size h) -> case (qtNode^.qtNodeQuadrant) of
						NWQuad -> px >= bx0 && py >= by0 && py - h < by1
						NEQuad -> traceShow ("ne: ", px, py, bx0, by0, bx1, by1) $ px < bx1 && py >= by0 && py - h < by1
						SWQuad -> px >= bx0 && py < by1 && py + h >= by0
						SEQuad -> px < bx1 && py < by1 && py + h >= by0
					(Size w, Infty) -> case (qtNode^.qtNodeQuadrant) of
						NWQuad -> px >= bx0 && py >= by0 && px + w < bx1
						NEQuad -> px < bx1 && py >= by0 && px - w >= bx0
						SWQuad -> px >= bx0 && py < by1 && px + w < bx1
						SEQuad -> px < bx1 && py < by1 && px - w >= bx0
					(Size w, Size h) -> case (qtNode^.qtNodeQuadrant) of 
						NWQuad -> px >= bx0 && py >= by0 && px + w < bx1 && py - h < by1
						NEQuad -> px < bx1 && py >= by0 && px - w >= bx0 && py - h < by1
						SWQuad -> px >= bx0 && py < by1 && px + w < bx1 && py + h >= by0
						SEQuad -> px < bx1 && py < by1 && px - w >= bx0 && py + h >= by0

				if cond then 
					if (Set.size $ qtNode^.qtNodeData) >= 4 
						then
							qtAddSubNodes nodeData qtNode
						else do
							qtNodes.at (qtNode^.qtNodeId) .= (Just $ qtNode
									& qtNodeData %~ Set.insert nodeDataId
								)
							qtNodeDatas . at nodeDataId %= (\(Just nodeData) -> Just $ nodeData
									& ndNodes %~ Set.insert (qtNode^.qtNodeId)
								)
					else
						return ()

			Nothing -> do
				if (Set.size $ qtNode^.qtNodeData) >= 4 
					then
						qtAddSubNodes nodeData qtNode
					else do
						qtNodes.at (qtNode^.qtNodeId) .= (Just $ qtNode
								& qtNodeData %~ Set.insert nodeDataId
							)
						qtNodeDatas . at nodeDataId %= (\(Just nodeData) -> Just $ nodeData
								& ndNodes %~ Set.insert (qtNode^.qtNodeId)
							)
		--Just nodeData <- use $ qtNodeDatas . at nodeDataId


	where
		nodeDataId = nodeData^.ndId

qtAddSubNodes nodeData qtNode = traceShow ("qtAddSubNodes") $ do
	let moveData = nodeDataId : (Set.toList $ qtNode^.qtNodeData)
	Just nodeData <- use $ qtNodeDatas . at nodeDataId
	let boundary = nodeData^.ndBoundary
	(cx, cy, nws, nes, sws, ses) <- newChildDimensions qtNode boundary
	nwId <- qtNewId
	neId <- qtNewId
	swId <- qtNewId
	seId <- qtNewId

	let (ncnnw, ncnne, ncnsw, ncnse) =
		( newChildNode nwId qtNode NWQuad nws, newChildNode neId qtNode NEQuad nes
		, newChildNode swId qtNode SWQuad sws, newChildNode seId qtNode SEQuad ses
		)

	qtNodes . at (ncnnw^.qtNodeId) .= (Just ncnnw)
	qtNodes . at (ncnne^.qtNodeId) .= (Just ncnne)
	qtNodes . at (ncnsw^.qtNodeId) .= (Just ncnsw)
	qtNodes . at (ncnse^.qtNodeId) .= (Just ncnse)


	qtNodes.at (qtNode^.qtNodeId) %= \(Just qtNode) -> (Just $ qtNode
			& qtNodeData .~ Set.empty	
			& qtProxy .~ True
			& qtNodeChilds .~ (Just $
					(cx, cy
					, ncnnw^.qtNodeId, ncnne^.qtNodeId
					, ncnsw^.qtNodeId, ncnse^.qtNodeId
					)
				)
		)	

	qtNodeDatas.at nodeDataId %= \(Just oldData) -> Just $ oldData
		& ndNodes %~ (\oldSet -> Set.delete (qtNode^.qtNodeId) oldSet)

	-- add moved data
	mapM_ (\ndId' -> do
			Just nodeData <- use $ qtNodeDatas . at ndId'
			Just qtNode' <- use (qtNodes.at (qtNode^.qtNodeId))
			qtNodeInsert nodeData qtNode'
		) moveData
	where
		nodeDataId = nodeData^.ndId

isLeaf qtNode = isNothing $ qtNode^.qtNodeChilds

-- | query all leaf nodes containing any part of the boundary
qtQueryLeafNodes boundary qtNode qt results
	| isLeaf qtNode = qtNode:results
	| allNW boundary center = 
		qtQueryLeafNodes boundary nw qt results
	| allNE boundary center =
		qtQueryLeafNodes boundary ne qt results
	| allSW boundary center =
		qtQueryLeafNodes boundary sw qt results
	| allSE boundary center =
		qtQueryLeafNodes boundary se qt results
	| allN boundary center =
		let results1 = qtQueryLeafNodes boundary nw qt results
		in qtQueryLeafNodes boundary ne qt results1
	| allS boundary center =
		let results1 = qtQueryLeafNodes boundary sw qt results
		in qtQueryLeafNodes boundary se qt results1
	| allE boundary center =
		let results1 = qtQueryLeafNodes boundary se qt results
		in qtQueryLeafNodes boundary ne qt results1
	| allW boundary center =
		let results1 = qtQueryLeafNodes boundary sw qt results
		in qtQueryLeafNodes boundary nw qt results1
	| otherwise =
		let 
			results1 = qtQueryLeafNodes boundary nw qt results
			results2 = qtQueryLeafNodes boundary sw qt results1
			results3 = qtQueryLeafNodes boundary ne qt results2
		in qtQueryLeafNodes boundary se qt results3
	
	where
		Just center = qtNode^.qtNodeCenter
		Just nw = qt^.qtNodes.at (fromJust $ qtNode^.qtNodeChildNW)
		Just ne = qt^.qtNodes.at (fromJust $ qtNode^.qtNodeChildNE)
		Just sw = qt^.qtNodes.at (fromJust $ qtNode^.qtNodeChildSW)
		Just se = qt^.qtNodes.at (fromJust $ qtNode^.qtNodeChildSE)

qtInsert oId boundary = do
		newId <- qtNewId
		let nodeData = NodeData
			{ _ndId = newId
			, _ndObjectId = oId
			, _ndBoundary = boundary
			, _ndNodes = Set.empty
			}
		qt' <- get
		let leafNodes = qtQueryLeafNodes boundary (qt'^?!qtNodes.at 1._Just) qt' []
		qtNodeDatas . at newId .= Just (nodeData
			)

		qt'' <- get
		mapM_ (\node -> qtNodeInsert nodeData node) leafNodes

		return ()

test = do
	let qt = emptyQuadtree

	let b0 = newBoundary (10, 10) (50, 50)
	let b1 = newBoundary (-30, -30) (50, 50)
	let b2 = newBoundary (0, 0) (1, 1)
	let b3 = newBoundary (1, 0) (1, 1)
	let b4 = newBoundary (2, 0) (1, 1)
	let b5 = newBoundary (3, 0) (1, 1)

	let qt' = execState (do
		qtInsert 1 b0
		qtInsert 1 b1
		qtInsert 1 b2 
		qtInsert 1 b3 
		qtInsert 1 b4 
		qtInsert 1 b5 
		) qt
	mapM_ print (Map.toList $ qt'^.qtNodeDatas)
	mapM_ print (Map.toList $ qt'^.qtNodes)
