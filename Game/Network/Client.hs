{-# LANGUAGE Rank2Types #-}
module Game.Network.Client 
	(
	  consumeClientWorld
	, decodeSteps
	) where

import Game.World

import Control.Monad.RWS
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad as CM

--import Control.Wire
import qualified Control.Wire as W

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Binary
 
import Network.Simple.TCP
import Control.Concurrent
 
import Pipes as P
import Pipes.Network.TCP
import Pipes.Concurrent
import Control.Concurrent.Async
import Pipes.Binary
import Game.Input.Actions
import qualified Game.Input.Actions as A
import Game.Render
import Game.Render.Map
import Game.World.Import.Tiled
import Data.Tiled
import qualified Data.Tiled as T
import Control.Concurrent.STM   (TQueue, TVar, readTVar, writeTVar, atomically, newTQueueIO, tryReadTQueue, writeTQueue, readTQueue)
import Control.Lens
import Data.Maybe
import qualified Data.Map as Map
import Game.World.Delta
import Game.World.Objects
import Game.World.Types
type ProdDecoder a = (Monad m)	 
	=> Producer B.ByteString m r
	-> Producer' (ByteOffset, a) m (Either (DecodingError, Producer B.ByteString m r) r)

decodeSteps :: ProdDecoder ([A.Action], Rational)
decodeSteps = decodeMany

clientStepWorld :: 
	   WorldWire () b 
	-> World 
	-> WorldManager 
	-> Rational 
	-> IO (WorldWire () b, (WorldManager, WorldDelta))
clientStepWorld w' world' state' dt' = do
	let dt = W.Timed (fromRational dt') ()
	-- run wires
	((out, w), worldManager, worldDelta) <- runRWST (
		W.stepWire w' dt (Right ())
		) world' state'

	return (w, (worldManager, worldDelta))

consumeClientWorld :: 
	   World 
	-> WorldManager 
	-> WorldWire () b 
	-> TVar RenderContext
	-> Consumer (ByteOffset, ([A.Action], Rational)) IO r
consumeClientWorld world manager w renderContextVar = do
	-- run wires
	(_, (actions, dt)) <- await

	let playerId = fromJust $ world^.wPlayerId "Neira"
	let playerActions = if Map.member playerId (manager^.wmPlayerActions)
		then (manager^.wmPlayerActions) Map.! playerId
		else mempty

	--let manager2 = manager  &wmPlayerActions %~ Map.insert playerId (A.newInputAction action)
	let manager2 = manager & wmPlayerActions %~	Map.insert playerId (
			playerActions `mappend` 
				foldr (\action as -> as `mappend` A.newInputAction action) mempty actions
		)
	(w', (manager', delta)) <- lift $ clientStepWorld w world manager2 dt

	 --update our world state
	let world' = applyDelta world delta

	let (Just pId) = world'^.wPlayerId "Neira"
	let playerPos = world'^.wPlayerPos "Neira"
	let playerGid = world'^.wObjectAnim pId.animTileGid
	let boulderPos = world'^.wBoulderPos "Boulder1"

	let (Just dinoId) = world'^.wPlayerId "Dino"
	let (Just beeId) = world'^.wPlayerId "Bee"
	let dinoPos = world'^.wPlayerPos "Dino"
	let beePos = world'^.wPlayerPos "Bee"
	let dinoGid = world'^.wObjectAnim dinoId.animTileGid
	let beeGid = world'^.wObjectAnim beeId.animTileGid

	let newObjects = delta^.wdObjectsAdd
	let objectGids = [world'^.wObjectAnim (o^.objId).animTileGid | o <- newObjects]
	let objectPoss = [world'^.wObjectPos' (o^.objId) | o <- newObjects]

	--lift $ print playerPos
	--lift $ print $ world'^.wAnimations
	--lift $ print $ delta^.wdAnimations

	lift $ atomically $ do
		renderContext <- readTVar renderContextVar
		let tm = renderContext^.rcWorldRenderContext.wrcMap.tiledMap
		let newRenderContext = execState (do
				mapM_ (\(obj, objGid, Just (x, y)) -> layerObj.layerObjects <>= 
						[T.Object { _objectName=Just $ obj^.objName
							   , _objectGid=Just (fromIntegral objGid)
							   , _objectX = fromIntegral . round $ x
							   , _objectY = fromIntegral . round $ y
							   , _objectWidth = Nothing
							   , _objectHeight = Nothing
							   }]
					) $ zip3 newObjects objectGids objectPoss
				case playerPos of
					Just (px, py) -> do
						tMap.object "Player1".objectPos tm .= (fromJust playerPos)
						tMap.object "Dino".objectPos tm .= (fromJust dinoPos)
						tMap.object "Bee".objectPos tm .= (fromJust beePos)
						--tMap.object "Player1".objectX .= round px
						--tMap.object "Player1".objectY .= round py
						tMap.object "Player1".objectGid .= Just (fromIntegral playerGid)
						tMap.object "Dino".objectGid .= Just (fromIntegral dinoGid)
						tMap.object "Bee".objectGid .= Just (fromIntegral beeGid)
					Nothing -> return ()
				case boulderPos of
					Just (px, py) ->
						tMap.object "Boulder1".objectPos tm .= (px, py)
					Nothing -> return ()
			) renderContext
		writeTVar renderContextVar newRenderContext


	-- repeat
	consumeClientWorld world' manager' w' renderContextVar

	where
		tMap :: Traversal' RenderContext TiledMap
		tMap = rcWorldRenderContext.wrcMap.tiledMap
		layerObj :: Traversal' RenderContext Layer
		layerObj = tMap.mapLayers.traverse._ObjectLayer
		--layerObj1 :: Traversal' RenderContext [Layer]
		object name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.objectsByName name

