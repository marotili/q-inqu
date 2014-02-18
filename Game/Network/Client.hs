{-# LANGUAGE Rank2Types #-}
module Game.Network.Client 
	(
	  consumeClientWorld
	, decodeSteps
	) where

import Game.World

import Control.Monad.RWS

--import Control.Wire
import qualified Control.Wire as W

import qualified Data.ByteString as B
 
import Pipes as P
import Pipes.Concurrent
import Pipes.Binary
import qualified Game.Input.Actions as A
import Game.Render
import Game.Render.Map
import Data.Tiled
import Control.Concurrent.STM ( TVar, readTVar, writeTVar)
import Control.Lens
import qualified Data.Map as Map
import Game.World.Objects
import Game.World.Common
import Game.Render.Update

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
	((_, w), worldManager, worldDelta) <- runRWST (
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
	--lift $ print ("Num wires", Map.size $ world'^.wCommon.wcWires)

	renderContext <- lift $ atomically $
		readTVar renderContextVar

	let renderWorld = renderContext^.rcWorldRenderContext.wrcWorld
	--let newTm = updateTiled world' delta tm

	(_, renderWorld2, newRenderables) <- lift $ runRWST (do
			updateTiled
			newRenderObjects
		) (world', delta, renderablesIn) renderWorld

	(_, renderWorld3, _) <- lift $ runRWST update
		(world', delta, renderablesIn ++ newRenderables) renderWorld2

	let updatedRenderContext = renderContext 
		& rcWorldRenderContext.wrcWorld .~ renderWorld3

	lift $ atomically $
		writeTVar renderContextVar updatedRenderContext

	-- repeat
	--lift $ performGC
	consumeClientWorld world' manager' w' renderContextVar (newRenderables ++ renderablesIn)

	where
		--tMap :: Traversal' RenderContext TiledMap
		--tMap = rcWorldRenderContext.wrcMap.tiledMap
		--layerObj :: Traversal' RenderContext Layer
		--layerObj = tMap.mapLayers.traverse._ObjectLayer
		--layerObj1 :: Traversal' RenderContext [Layer]
		--object name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.objectsByName name

