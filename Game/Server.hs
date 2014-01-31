{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game.World


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

produceWorld :: World -> WorldManager -> WorldWire () b -> WorldSession -> 
	Producer (Rational, WorldDelta, World) IO ()
produceWorld world manager w session = do
	-- run wires
	((w', session'), (manager', delta), dt) <- lift $ stepWorld w session world manager

	-- update our world state
	let world' = applyDelta world delta

	-- debug output
	lift $ print world'

	-- Send to user
	P.yield (realToFrac dt, delta, world')

	-- wait
	lift $ threadDelay oneSecond

	-- repeat
	produceWorld world' manager' w' session'

--getDeltaTime (dt, deltaWorld, world) = dt
--getWorld (dt, dWorld, world) = world

game recvEvents output = do
	let session = clockSession_
	let world = newWorld
	runEffect $
		--fromInput recvEvents >->
		P.for (produceWorld world newWorldManager testwire session) PB.encode 
		>-> toOutput output
	performGC
 
main = withSocketsDo $ do

	(output1, input1) <- spawn Unbounded
	(output2, input2) <- spawn Unbounded

	(sendEvents, recvEvents) <- spawn Unbounded

	let output = output1 Monoid.<> output2
	a1 <- async $ game recvEvents output

	serve HostIPv4 "5002" (connCb (sendEvents, input1, input2))
	mapM_ wait [a1]

	return ()

connCb (sendEvents, input1, input2) (sock, addr) = do
	print addr
	--sClose sock

	let cons = toSocket sock
	--let prod = fromSocket sock 4096
	--runEffect $ (P.yield "Test" >-> cons)
	a1 <- async $ do
		runEffect $ fromInput input1 >-> cons
		performGC
	a2 <- async $ do
		--runEffect $ P.for prod (P.lift . print)
		performGC

	mapM_ wait (a1:[a2])

	return ()

