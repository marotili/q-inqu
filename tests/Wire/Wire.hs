module Wire where

import Test.HUnit
import Game.Render.World
import Control.Lens
import Control.Concurrent

import Control.Wire

import Game.World.Wires

testLoadMap :: Test
testLoadMap = TestCase (do
		let session = clockSession_
		gameLoop session (Wire.testWire)
	)

testWire :: Wire (Timed NominalDiffTime ()) () IO Float NominalDiffTime
testWire = saveFor 2 (time)

gameLoop session w = do
	(dt, session') <- stepSession session

	(mx, w') <- stepWire w dt (Right 0)
	print mx

	threadDelay 100

	case mx of 
		Left _ -> return ()
		Right _ -> gameLoop session' w'
