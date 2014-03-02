{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Main where

import Debug.Trace
import Game.World

import Control.Monad.RWS.Strict
--import Control.Monad.Identity
--import Control.Monad.Reader
--import Control.Monad.Writer
import Control.Monad as CM

--import qualified Data.Binary as B

import Control.Wire
import qualified Prelude as P
import Prelude hiding ((.), until)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Network hiding (accept, sClose)
import Network.Socket
import Control.Concurrent
import Network.Simple.TCP
import Control.Concurrent.Async
import qualified Data.Monoid as Monoid

import Control.Concurrent.STM
import Pipes as P
import Pipes.Network.TCP
import Pipes.Concurrent
import Control.Lens
import qualified Data.Set as Set
import qualified Pipes.Binary as PB

import Game.World.Common
import Game.Game
import Control.Monad.State.Strict
import Data.Tiled

import qualified Game.Input.Actions as A
import Game.World.AI.Basic
import Game.Network.Client

type ProdDecoder a = (Monad m)	 
	=> Producer B.ByteString m r
	-> Producer' (PB.ByteOffset, a) m (Either (PB.DecodingError, Producer B.ByteString m r) r)
decodeAction :: ProdDecoder FromClientData
decodeAction = PB.decodeMany




type FromClientData = (Float, Int, A.Action)
type ClientData = ([(Int, A.Action)], Rational)

collect :: [(Int, A.Action)] -> Pipe FromClientData ClientData IO [(Int, A.Action)]
collect actions = do
	(_, pId, action) <- P.await
	if action == A.ActionUpdateGameState then
		return (actions ++ [(pId, action)])
	else
		collect (actions ++ [(pId, action)])

--produceWorld :: WorldSession ->
	--Pipe FromClientData ClientData IO ()
produceWorld session' lastDt total = do
	--(t, action) <- P.await
	actions <- collect []
	(dt, session) <- lift $ stepSession session'

	let time = dt
	let 
		finalTime :: Rational
		finalTime = (realToFrac . dtime $ time) + lastDt


	let steps = traceShow (realToFrac . dtime $ time) [16.0/1000 | _ <- [0..(floor (finalTime/16*1000) - 1)]]
	let final = foldr (\_ time -> if time > 16/1000.0 then (time - 16/1000.0) else time) finalTime [0..floor (finalTime/16*1000)]

	mapM_ (\time -> P.yield (actions, realToFrac time)) $ traceShow (steps, realToFrac final) steps
	let total' = if length steps > 1 then
		total + 1 else total
	lift $ writeFile "time.log" (show total')
	final' <- if final > 15/1000 then do
		P.yield (actions, realToFrac final)
		return 0
	else
		return final

	traceShow ("step") $ produceWorld session final' total'
--getDeltaTime (dt, deltaWorld, world) = dt
--getWorld (dt, dWorld, world) = world

--debug = do
--	p <- P.await
--	lift $ print p
--	P.yield p

fakeClient game = do
		(_, (actions, dt)) <- await
		let manager2 = worldManagerUpdate (game^.gameWorldManager) actions

		let ((actions1, actions2), newGame) = runState (do
				gameWorldManager .= manager2
				updateGame dt
				world <- use gameLogicWorld 
				worldDelta <- use gameLastDelta
				manager <- use gameWorldManager

				let A.InputActions aiInput1 = runAI 3 world worldDelta dt
				let A.InputActions aiInput2 = runAI 4 world worldDelta dt
				return (aiInput1, aiInput2)
			) game

		mapM_ (\a -> P.yield (0, 3, a)) $ Set.toList actions1 
		mapM_ (\a -> P.yield (0, 4, a)) $ Set.toList actions2
		fakeClient newGame

runFakeClient input output game = 
	void (decodeSteps (fromInput input)) >-> fakeClient game >-> toOutput output

runGame :: Input FromClientData -> Output C.ByteString -> IO ()
runGame recvEvents output = do
	let session = clockSession_


	let
		worldProducer :: Pipe (Float, Int, A.Action) ClientData IO ()
		worldProducer = produceWorld session (0::Rational) 0

		--eventProducer :: Producer (Float, A.Action) IO ()
		eventProducer = P.for (fromInput recvEvents) P.yield
	runEffect $
		P.for (eventProducer >-> worldProducer) PB.encode 
			>-> toOutput output
	performGC

--eventUpdate :: Producer (Float, Int, A.Action) IO ()
eventUpdate n = do
	P.yield (0, -1, A.ActionUpdateGameState)
	let n' = if n > 10 then 0 else n + 1
	if n' == 0 then return ()
		--lift $ performGC
		else return ()
	lift $ threadDelay (1000000 `div` 60)
	eventUpdate n'
 
main :: IO ()
main = withSocketsDo $ do
	(output1, input1) <- spawn Unbounded
	(output2, input2) <- spawn Unbounded
	(output3, input3) <- spawn Unbounded

	(sendEvents1, recvEvents1) <- spawn Unbounded :: IO (Output FromClientData, Input FromClientData)
	--(sendEvents2, recvEvents2) <- spawn Unbounded
	--(sendEvents3, recvEvents3) <- spawn Unbounded

	numClient <- newTVarIO (0 :: Int)

	let recvEvents = recvEvents1
	--let sendEvents = sens
	let output = output1 Monoid.<> output2 Monoid.<> output3

	_ <- async $ do
		runEffect $ eventUpdate 0 >-> toOutput sendEvents1
		performGC

	game <- newGame "test"

	a1 <- async $ runGame recvEvents output
	_ <- async $ do
		runEffect $ runFakeClient input3 sendEvents1 game
		performGC

	serve HostIPv4 "5002" (connCb (numClient, sendEvents1, input1, input2))

	wait a1

	return ()


forward :: Pipe (PB.ByteOffset, (Float, Int, A.Action)) (Float, Int, A.Action) IO ()
forward = do
	(off, d) <- P.await
	--lift $ print (off, d)
	CM.unless (off < 0) $ do
		P.yield d
		forward

--connCb :: (Output (Float, A.Action), Input C.ByteString, Input C.ByteString)
	-- -> (Socket, SockAddr) 
	 -- > IO ()
connCb :: (TVar Int, Output (Float, Int, A.Action), Input C.ByteString, Input C.ByteString)
	-> (Socket, SockAddr) -> IO ()
connCb (numClient, sendEvents, input1, input2) (sock, _) = do
	--print "Server info: player joined"
	cl <- atomically $ do
		n <- readTVar numClient
		if n == 2 then
			return (-1)
		else do
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
		_ <- async $ do
			runEffect $ fromInput (if cl == 0 then input1 else input2) >-> toClient
			performGC
		a2 <- async $ do
			runEffect $ testX >-> forward >-> toOutput sendEvents
			performGC

		--print "Wait for quit"

		mapM_ wait [a2]

		_ <- atomically $ do
			n <- readTVar numClient
			writeTVar numClient (n-1)
			return n

		return ()

	--print ("Server info: player left", cl)
