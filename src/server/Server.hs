{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Main where

import Game.World

import Pipes.Lift


import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
--import Control.Monad.Identity
--import Control.Monad.Reader
--import Control.Monad.Writer
import Control.Monad.State
import qualified Control.Monad.State as State
import Control.Monad as CM

--import qualified Data.Binary as B

import Control.Wire
import qualified Control.Wire as W
import Control.Wire.Unsafe.Event
import qualified Prelude as P
import Prelude hiding ((.), until)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C

import Control.Lens

import Network hiding (accept, sClose)
import Network.Socket
import Control.Concurrent
import Network.Simple.TCP
import Control.Concurrent.Async
import qualified Data.Monoid as Monoid

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Pipes as P
import Pipes.Network.TCP
import Pipes.Concurrent
import qualified Pipes.Binary as PB

import Game.World.Import.Tiled
import Game.World.Common
import Data.Tiled

import qualified Game.Input.Actions as A
import Data.Maybe
import Data.Binary

oneSecond = 1000000
millisecond = oneSecond `div` 1000

type ProdDecoder a = (Monad m)	 
	=> Producer B.ByteString m r
	-> Producer' (PB.ByteOffset, a) m (Either (PB.DecodingError, Producer B.ByteString m r) r)
decodeAction :: ProdDecoder FromClientData
decodeAction = PB.decodeMany


stepWorld :: WorldWire () b -> WorldSession -> World -> WorldManager -> 
	IO ((WorldWire () b, WorldSession), (WorldManager, WorldDelta), NominalDiffTime)
stepWorld w' session' world' state' = do
	-- update session
	(dt, session) <- stepSession session'

	-- run wires
	((out, w), worldManager, worldDelta) <- runRWST (
		stepWire w' dt (Right ())
		) world' state'

	return ((w, session), (worldManager, worldDelta), dtime dt)

type FromClientData = (Float, Int, A.Action)
type ClientData = ([(Int, A.Action)], Rational)

collect actions = do
	(t, pId, action) <- P.await
	if action == A.ActionUpdateGameState then
		return (actions ++ [(pId, action)])
	else
		collect (actions ++ [(pId, action)])

produceWorld :: World -> WorldManager -> WorldWire () b -> WorldSession ->
	Pipe FromClientData ClientData IO ()
produceWorld world manager w session = do
	--(t, action) <- P.await
	actions <- collect []
	--lift $ print actions
	--let playerId = fromJust $ world^.wPlayerId "Neira"
	let manager2 = worldManagerUpdate manager actions

	--let manager2 = manager & wmPlayerActions %~	Map.insert playerId (A.newInputAction action)
	-- run wires
	--lift $ print "run main loop"
	--lift $ print manager2
	((w', session'), (manager', delta), dt) <- lift $ stepWorld w session world manager2
	--lift $ print "update clients"
	-- update our world state
	let world' = applyDelta world delta
	--lift $ print world'

	-- debug output
	--lift $ print world'

	-- Send to user
	--lift $ print actions
	P.yield (actions, realToFrac dt)

	-- wait
	--lift $ threadDelay oneSecond

	-- repeat
	produceWorld world' manager' w' session'

--getDeltaTime (dt, deltaWorld, world) = dt
--getWorld (dt, dWorld, world) = world

--debug = do
--	p <- P.await
--	lift $ print p
--	P.yield p

game :: Input FromClientData -> Output C.ByteString -> IO ()
game recvEvents output = do
	let session = clockSession_
	tiledMap <- loadMapFile "data/sewers.tmx"
	(world, manager) <- newWorldFromTiled tiledMap

	let
		worldProducer :: Pipe (Float, Int, A.Action) ClientData IO ()
		worldProducer = produceWorld world manager testwire session

		--eventProducer :: Producer (Float, A.Action) IO ()
		eventProducer = P.for (fromInput recvEvents) P.yield
	runEffect $
		P.for (eventProducer >-> worldProducer) PB.encode 
			>-> toOutput output
	performGC

eventUpdate = do
	P.yield (0, -1, A.ActionUpdateGameState)
	lift $ threadDelay (oneSecond `div` 30)
	eventUpdate
 
main = withSocketsDo $ do
	(output1, input1) <- spawn Unbounded
	(output2, input2) <- spawn Unbounded

	(sendEvents1, recvEvents1) <- spawn Unbounded :: IO (Output FromClientData, Input FromClientData)
	--(sendEvents2, recvEvents2) <- spawn Unbounded
	--(sendEvents3, recvEvents3) <- spawn Unbounded

	numClient <- newTVarIO (0 :: Int)

	let recvEvents = recvEvents1
	--let sendEvents = sens
	let output = output1 Monoid.<> output2

	async $ do
		runEffect $ eventUpdate >-> toOutput sendEvents1
		performGC

	a1 <- async $ game recvEvents output

	serve HostIPv4 "5002" (connCb (numClient, sendEvents1, input1, input2))
	mapM_ wait [a1]

	return ()


forward :: Pipe (PB.ByteOffset, (Float, Int, A.Action)) (Float, Int, A.Action) IO ()
forward = do
	(off, d@(dt, pId, actions)) <- P.await
	--lift $ print (off, d)
	CM.unless (off < 0) $ do
		P.yield d
		forward

--connCb :: (Output (Float, A.Action), Input C.ByteString, Input C.ByteString)
	-- -> (Socket, SockAddr) 
	-- -> IO ()
connCb (numClient, sendEvents, input1, input2) (sock, addr) = do
	--print "Server info: player joined"
	cl <- atomically $ do
		n <- readTVar numClient
		if n == 2 then
			return (-1)
		else do
			if n + 1 == 2 then return () else return ()
				-- send game start

			writeTVar numClient (n+1)
			return n

	CM.unless (cl == -1) $ do
		let toClient = toSocket sock
		let toClientFirst = toSocket sock
		--print "Send player id"
		runEffect $ PB.encode cl >-> toClientFirst
		--print "Sent player id"

		let fromClient = fromSocket sock 4096
		let
			--testX :: Producer (PB.ByteOffset, (Float, Int, A.Action)) IO ()
			testX = decodeAction fromClient >>= (\e ->
					case e of
						Right _ -> do
							P.yield (-1, (0, -1, A.ActionNothing))
							return ()
						Left _ -> do
							P.yield (-2, (0, -1, A.ActionNothing))
							return ()
				)

		--runEffect $ (P.yield "Test" >-> cons)
		a1 <- async $ do
			runEffect $ fromInput (if cl == 0 then input1 else input2) >-> toClient
			performGC
		a2 <- async $ do
			runEffect $ testX >-> forward >-> toOutput sendEvents
			performGC

		--print "Wait for quit"

		mapM_ wait [a2]

		cl <- atomically $ do
			n <- readTVar numClient
			writeTVar numClient (n-1)
			return n

		return ()

	--print ("Server info: player left", cl)
