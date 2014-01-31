{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Main where

import Game.World

import Pipes.Lift


import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad as CM

import qualified Data.Binary as B

import Control.Wire
import qualified Control.Wire as W
import Control.Wire.Unsafe.Event
import qualified Prelude as P
import Prelude hiding ((.), until)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C

import Data.Binary
 
import Network hiding (accept, sClose)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Control.Concurrent
import Network.Simple.TCP
import Control.Concurrent.Async
import qualified Data.Monoid as Monoid

import Control.Concurrent.STM
import Pipes as P
import Pipes.Network.TCP
import Pipes.Concurrent
import qualified Pipes.Binary as PB

import Game.Input.Actions

oneSecond = 1000000
millisecond = oneSecond `div` 1000

type ProdDecoder a = (Monad m)	 
	=> Producer B.ByteString m r
	-> Producer' (PB.ByteOffset, a) m (Either (PB.DecodingError, Producer B.ByteString m r) r)
decodeAction :: ProdDecoder (Float, Action)
decodeAction = PB.decodeMany


stepWorld :: WorldWire () b -> WorldSession -> World -> WorldManager -> 
	IO ((WorldWire () b, WorldSession), (WorldManager, WorldDelta), NominalDiffTime)
stepWorld w' session' world' state' = do
	-- update session
	(dt, session) <- stepSession session'

	print dt

	-- run wires
	((out, w), worldManager, worldDelta) <- runRWST (
		stepWire w' dt (Right ())
		) world' state'

	return ((w, session), (worldManager, worldDelta), dtime dt)

type ClientData = ((Float, Action), Rational, WorldDelta, World)

produceWorld :: World -> WorldManager -> WorldWire () b -> WorldSession ->
	Pipe (Float, Action) ClientData IO ()
produceWorld world manager w session = do
	(t, action) <- P.await

	let playerId = 1
	let playerActions = if Map.member playerId (wmPlayerActions manager)
		then wmPlayerActions manager Map.! playerId
		else mempty

	--lift $ print playerActions
	--lift $ print action
	let manager2 = manager { wmPlayerActions = 
		Map.insert playerId (playerActions `mappend` (newInputAction action)) (wmPlayerActions manager) 
		}
	-- run wires
	((w', session'), (manager', delta), dt) <- lift $ stepWorld w session world manager2

	-- update our world state
	let world' = applyDelta world delta
	lift $ print world'

	-- debug output
	--lift $ print world'

	-- Send to user
	P.yield ((t, action), realToFrac dt, delta, world')

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

game :: Input (Float, Action) -> Output C.ByteString -> IO ()
game recvEvents output = do
	let session = clockSession_
	let world = newWorld

	let
		worldProducer :: Pipe (Float, Action) ClientData IO ()
		worldProducer = produceWorld world newWorldManager testwire session

		eventProducer :: Producer (Float, Action) IO ()
		eventProducer = P.for (fromInput recvEvents) (\a -> lift (print a) >> P.yield a)
	runEffect $
		P.for (eventProducer >-> worldProducer) PB.encode 
			>-> toOutput output
	performGC

eventUpdate = do
	P.yield (0, ActionNothing)
	lift $ threadDelay (millisecond*100)
	eventUpdate
 
main = withSocketsDo $ do
	(output1, input1) <- spawn Unbounded
	(output2, input2) <- spawn Unbounded

	(sendEvents1, recvEvents1) <- spawn Unbounded :: IO (Output (Float, Action), Input (Float, Action))
	--(sendEvents2, recvEvents2) <- spawn Unbounded
	--(sendEvents3, recvEvents3) <- spawn Unbounded

	let recvEvents = recvEvents1
	--let sendEvents = sens
	let output = output1 Monoid.<> output2

	async $ do
		runEffect $ eventUpdate >-> toOutput sendEvents1
		performGC

	a1 <- async $ game recvEvents output

	serve HostIPv4 "5002" (connCb (sendEvents1, input1, input2))
	mapM_ wait [a1]

	return ()


forward :: Pipe (PB.ByteOffset, (Float, Action)) (Float, Action) IO ()
forward = do
	(_, d) <- P.await
	lift $ print d
	P.yield d
	forward

connCb :: (Output (Float, Action), Input C.ByteString, Input C.ByteString)
	-> (Socket, SockAddr) 
	-> IO ()
connCb (sendEvents, input1, input2) (sock, addr) = do
	print addr
	--sClose sock

	let toClient = toSocket sock
	let fromClient = fromSocket sock 4096

	let
		testX :: Producer (PB.ByteOffset, (Float, Action)) IO ()
		testX = (decodeAction fromClient >> return () >> testX) 

	--runEffect $ (P.yield "Test" >-> cons)
	a1 <- async $ do
		runEffect $ fromInput input1 >-> toClient
		performGC
	a2 <- async $ do
		runEffect $ testX >-> forward >-> toOutput sendEvents
		performGC


	mapM_ wait (a1:[a2])

	return ()
	where


