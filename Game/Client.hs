{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Game.Client where

import Game.World

import Control.Monad.RWS
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad as CM

import Control.Wire
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

type ProdDecoder a = (Monad m)	 
	=> Producer B.ByteString m r
	-> Producer' (ByteOffset, a) m (Either (DecodingError, Producer B.ByteString m r) r)
decodeDoubles :: ProdDecoder Double
decodeDoubles = decodeMany

decodeWorld :: ProdDecoder World
decodeWorld = decodeMany

decodeSteps :: ProdDecoder (Rational, WorldDelta, World)
decodeSteps = decodeMany

clientStepWorld :: WorldWire () b ->  World -> WorldManager -> Rational ->
	IO (WorldWire () b, (WorldManager, WorldDelta))
clientStepWorld w' world' state' dt' = do
	let dt = Timed (fromRational dt') ()

	-- run wires
	((out, w), worldManager, worldDelta) <- runRWST (
		stepWire w' dt (Right ())
		) world' state'

	return (w, (worldManager, worldDelta))

consumeClientWorld :: World -> WorldManager -> WorldWire () b -> 
	Consumer (ByteOffset, (Rational, WorldDelta, World)) IO r
consumeClientWorld world manager w = do
	-- run wires
	(_, (dt, serverDelta, serverWorld)) <- await
	(w', (manager', delta)) <- lift $ clientStepWorld w world manager dt

	-- update our world state
	let world' = applyDelta world delta

	-- debug output
	lift $ print $ delta == serverDelta
	lift $ print $ world == serverWorld

	-- repeat
	consumeClientWorld world' manager' w'

--main = withSocketsDo $ 

