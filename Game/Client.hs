{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Game.Client where

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
import Game.Tiled
import Data.Tiled
import Control.Concurrent.STM   (TQueue, TVar, readTVar, writeTVar, atomically, newTQueueIO, tryReadTQueue, writeTQueue, readTQueue)
import Control.Lens
import Data.Maybe
import qualified Data.Map as Map

type ProdDecoder a = (Monad m)	 
	=> Producer B.ByteString m r
	-> Producer' (ByteOffset, a) m (Either (DecodingError, Producer B.ByteString m r) r)
decodeDoubles :: ProdDecoder Double
decodeDoubles = decodeMany

--decodeWorld :: ProdDecoder World
--decodeWorld = decodeMany

decodeSteps :: ProdDecoder ((Float, A.Action), Rational)
decodeSteps = decodeMany

clientStepWorld :: WorldWire () b ->  World -> WorldManager -> Rational ->
	IO (WorldWire () b, (WorldManager, WorldDelta))
clientStepWorld w' world' state' dt' = do
	let dt = W.Timed (fromRational dt') ()
	print $ "Server delta time" ++ show dt
	-- run wires
	((out, w), worldManager, worldDelta) <- runRWST (
		W.stepWire w' dt (Right ())
		) world' state'

	return (w, (worldManager, worldDelta))

consumeClientWorld :: World -> WorldManager -> WorldWire () b -> (TVar RenderContext)
	-> Consumer (ByteOffset, ((Float, A.Action), Rational)) IO r
consumeClientWorld world manager w renderContextVar = do
	-- run wires
	(_, ((userTime, action), dt)) <- await

	let playerId = fromJust $ world^.getPlayerId "Neira"
	let playerActions = if Map.member playerId (_wmPlayerActions manager)
		then _wmPlayerActions manager Map.! playerId
		else mempty

	let manager2 = manager { _wmPlayerActions = 
		Map.insert playerId (playerActions `mappend` (A.newInputAction action)) (_wmPlayerActions manager) 
		}

	(w', (manager', delta)) <- lift $ clientStepWorld w world manager2 dt
	lift $ print (userTime, action, dt)

	 --update our world state
	let world' = applyDelta world delta

	let playerPos = world'^.getPlayerPos "Neira"

	lift $ print playerPos
	lift $ print delta

	lift $ atomically $ do
		renderContext <- readTVar renderContextVar
		let newRenderContext = execState (do 
				case playerPos of
					Just (px, py) -> do
						tMap.object "Player1".objectX .= round(px)
						tMap.object "Player1".objectY .= round(py)
					Nothing -> return ()
			) (renderContext)
		writeTVar renderContextVar newRenderContext

	 --debug output

	-- repeat
	consumeClientWorld world' manager' w' renderContextVar
	--consumeClientWorld world manager w renderContextVar

	where
		tMap :: Traversal' RenderContext TiledMap
		tMap = rcWorldRenderContext.wrcMap.tiledMap
		--objectCoordX :: Traversal' TiledMap Int
		object name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.(objectsByName name)
		--objectCoordX name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.(objectsByName name).objectY
		-- .mapLayers.traverse._ObjectLayer.layerObjects.traverse.(objectsByName "Player1").objectX .~ 5

--main = withSocketsDo $ 

