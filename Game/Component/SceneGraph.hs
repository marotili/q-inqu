{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Game.Component.SceneGraph where

import Control.Lens
import Game.World.Objects

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State

type NodeId = Int
data SceneGraph = SceneGraph
	{ _sgNodes :: Map.Map NodeId SceneNode
	, _sgObjectNode :: Map.Map ObjectId NodeId
	, _sgNextId :: NodeId
	} deriving (Show)

data NodeData = NodeData
	{ _nodePosition :: Position
	--, _nodeOrientation :: Orientation
	} deriving (Show)

data SceneNode = SceneNode
	{ _nodeId :: NodeId
	, _nodeParentId :: Maybe NodeId
	, _nodeChildIds :: [NodeId]
	, _nodeObject :: Maybe ObjectId
	, _nodeData :: NodeData
	} deriving (Show)

makeLenses ''SceneGraph
makeLenses ''NodeData
makeLenses ''SceneNode

newSceneGraph :: SceneGraph
newSceneGraph = SceneGraph
	{ _sgNodes = Map.insert 0 newSceneNode Map.empty
	, _sgObjectNode = Map.empty
	, _sgNextId = 1
	}

newSceneNode :: SceneNode
newSceneNode = SceneNode
	{ _nodeId = 0
	, _nodeParentId = Nothing
	, _nodeChildIds = []
	, _nodeObject = Nothing
	, _nodeData = newNodeData
	}

newNodeData :: NodeData
newNodeData = NodeData
	{ _nodePosition = (0, 0)
	--, _nodeOrientation = 0
	}

--sgNewNode :: SceneGraph -> State SceneGraph SceneNode

sceneNode :: ObjectId -> Lens SceneGraph (Maybe SceneGraph) (Maybe SceneNode) SceneNode
sceneNode oId = lens (sgGetSceneNode oId) (sgSetSceneNode oId)

sgSetSceneNode :: ObjectId -> SceneGraph -> SceneNode -> Maybe SceneGraph
sgSetSceneNode oId sg sn 
	| isNothing (sn^.nodeParentId) = Nothing
	| sn^.nodeObject /= Just oId = Nothing
	| otherwise = Just $ execState (
		case mNodeId of
			-- just update node
			Just nodeId ->
				sgNodes.at nodeId .= Just sn

			-- insert node
			Nothing -> do
				nId <- use sgNextId
				sgNextId += 1
				sgObjectNode . at oId .= Just nId
				sgNodes . at nId .= Just (sn
					& nodeId .~ nId
					)
				sgNodes . at (fromJust $ sn^.nodeParentId) . _Just . nodeChildIds %= (++) [nId]	
	) sg
	where
		mNodeId = sg^.sgObjectNode.at oId

sgGetSceneNode :: ObjectId -> SceneGraph -> Maybe SceneNode
sgGetSceneNode oId sg = mNodeId >>= \nId -> node nId
	where
		mNodeId = sg^.sgObjectNode.at oId
		node nodeId = sg^.sgNodes.at nodeId -- node must exist

insert parentOId oId sg
	| parentOId == 0 = sg &
		sceneNode oId .~ (newSceneNode
			& nodeParentId .~ Just 0
			& nodeObject .~ (Just oId)
		)
	| otherwise = sg &
		sceneNode oId .~ (newSceneNode
			& nodeParentId .~ (Just parentNodeId)
			& nodeObject .~ (Just oId)
		)
	where
		parentNode :: SceneNode
		Just parentNode = sgGetSceneNode parentOId sg
		parentNodeId = parentNode^.nodeId

update oId nd sg = sg
	& sceneNode oId .~ (node
		& nodeData .~ nd
		)
	where Just node = sgGetSceneNode oId sg

test = do
	let sg = newSceneGraph
	let Just sg' = insert 0 1 sg
	let Just sg'' = insert 1 2 sg'

	let Just sg2 = update 1 (newNodeData & nodePosition .~ (50, 50)) sg''

	print sg2
	return ()


