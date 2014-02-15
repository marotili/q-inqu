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
import qualified Control.Monad.State as State
import Game.World.Types
import Game.World.Lens
import Game.World
import Game.World.Common
type ProdDecoder a = (Monad m)	 
	=> Producer B.ByteString m r
	-> Producer' (ByteOffset, a) m (Either (DecodingError, Producer B.ByteString m r) r)

decodeSteps :: ProdDecoder ([(PlayerId, A.Action)], Rational)
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
	-> [Renderable]
	-> Consumer (ByteOffset, ([(Int, A.Action)], Rational)) IO r
consumeClientWorld world manager w renderContextVar renderablesIn = do
	-- run wires
	(_, (actions, dt)) <- await

	let manager2 = worldManagerUpdate manager actions


	(w', (manager', delta)) <- lift $ clientStepWorld w world manager2 dt

	 --update our world state
	let world' = applyDelta world delta
	lift $ print ("Num wires", Map.size $ world'^.wCommon.wcWires)

	renderContext <- lift $ atomically $ do
		readTVar renderContextVar

	let tm = renderContext^.rcWorldRenderContext.wrcMap.tiledMap
	--let newTm = updateTiled world' delta tm

	(_, newTm2, newRenderables) <- lift $ runRWST (do
			updateTiled
			newRenderObjects
		) (world', delta, renderablesIn) tm

	(_, newTm3, _) <- lift $ runRWST (do
			update
		) (world', delta, renderablesIn ++ newRenderables) newTm2

	let newRenderContext = renderContext & tMap .~ newTm3

	lift $ atomically $ do
		writeTVar renderContextVar newRenderContext

	-- repeat
	--lift $ performGC
	consumeClientWorld world' manager' w' renderContextVar (newRenderables ++ renderablesIn)

	where
		tMap :: Traversal' RenderContext TiledMap
		tMap = rcWorldRenderContext.wrcMap.tiledMap
		layerObj :: Traversal' RenderContext Layer
		layerObj = tMap.mapLayers.traverse._ObjectLayer
		--layerObj1 :: Traversal' RenderContext [Layer]
		object name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.objectsByName name

