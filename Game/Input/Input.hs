{-# LANGUAGE Arrows #-}
{-# OPTIONS -Wall #-}

module Game.Input.Input 
	(
	-- * Input
	  UserInput
	, inputNew, inputKeyDown, inputKeyUp
	, inputMouseButtonDown, inputMouseButtonUp
	, inputUpdateMousePos

	, keyDown, keyUp
	, keyDownEvent, keyUpEvent

	-- * Wires
	, InputWire
	, stepInput, userInput

	-- * Utils
	, untilV
	) where

import Prelude hiding ((.))
import Control.Wire

-- for W.-->
import qualified Control.Wire as W 
import Control.Wire.Unsafe.Event

import qualified Data.Set as Set

import Control.Monad.State
import Control.Monad.RWS

import qualified Graphics.UI.GLFW as GLFW

import Game.Input.Actions

import Linear

-- input data
data UserInput = UserInput
  { inputKeys :: Set.Set GLFW.Key
  , inputMouseButtons :: Set.Set GLFW.MouseButton
  , inputMousePos :: (Double, Double)
  } deriving (Show)

data InputMemory = InputMemory
	{
	}

type InputContext = RWS UserInput InputActions InputMemory
type InputSession = Session IO (Timed NominalDiffTime ())
type InputWire a b = Wire (Timed NominalDiffTime ()) () InputContext a b

inputNew :: UserInput
inputNew = UserInput
  { inputKeys = Set.empty
  , inputMouseButtons = Set.empty
  , inputMousePos = (0, 0)
  }

inputMemoryNew :: InputMemory
inputMemoryNew = InputMemory {}

inputKeyDown :: GLFW.Key -> State UserInput ()
inputKeyDown k = modify $ \i -> i { inputKeys = Set.insert k (inputKeys i) }

inputKeyUp :: GLFW.Key -> State UserInput ()
inputKeyUp k = modify $ \i -> i { inputKeys = Set.delete k (inputKeys i) }

inputMouseButtonDown :: GLFW.MouseButton -> State UserInput ()
inputMouseButtonDown mb = modify $ \i -> i { inputMouseButtons = Set.insert mb (inputMouseButtons i) }

inputMouseButtonUp :: GLFW.MouseButton -> State UserInput ()
inputMouseButtonUp mb = modify $ \i -> i { inputMouseButtons = Set.delete mb (inputMouseButtons i) }

inputUpdateMousePos :: (Double, Double) -> State UserInput ()
inputUpdateMousePos pos = modify $ \i -> i { inputMousePos = pos }

-- * Basic wires

-- | Create a wire that triggers an event as soon as the monad action returns true
inputEvent :: InputContext Bool -> InputWire a (Event a)
inputEvent cond = mkGenN $ \a -> do
  eventHappened <- cond
  return $ if eventHappened
    then (Right (Event a), inputEvent cond)
    else (Right NoEvent, inputEvent cond)

-- | Create a wire that runs as long as the monad action returns true.
inputState :: InputContext Bool -> InputWire a a
inputState cond = mkGenN $ \a -> do
  eventHappened <- cond
  return $ if eventHappened
    then (Right a, inputState cond)
    else (Left () , inputState cond)

keyDownEvent :: GLFW.Key -> InputWire a (Event a)
keyDownEvent key = inputEvent (liftM (Set.member key . inputKeys) ask)

keyUpEvent :: GLFW.Key -> InputWire a (Event a)
keyUpEvent key = inputEvent (liftM (not . Set.member key . inputKeys) ask)

keyDown :: GLFW.Key -> InputWire a a
keyDown key = inputState (liftM (Set.member key . inputKeys) ask)

keyUp :: GLFW.Key -> InputWire a a
keyUp key = inputState (liftM (not . Set.member key . inputKeys) ask)

stepInput ::  
	   InputWire Int b 
	-> InputSession 
	-> State UserInput () 
	-> IO (InputActions, InputSession, InputWire Int b)
stepInput w' session' input = do
	(dt, session) <- stepSession session'
	let ((_, w), _, actions) = runRWS (
		stepWire w' dt (Right 0)
		) (execState input inputNew) inputMemoryNew

	return (actions, session, w)

-- * User input
directionX :: InputWire a (V2 Float)
directionX = pure (V2 1 0) . keyDown GLFW.Key'A . keyUp GLFW.Key'D <|> 
		pure (V2 (-1) 0) . keyDown GLFW.Key'D . keyUp GLFW.Key'A <|> 
		pure 0

directionY :: InputWire a (V2 Float)
directionY = pure (V2 0 (-1)) . keyDown GLFW.Key'W . keyUp GLFW.Key'S <|> 
		pure (V2 0 1) . keyDown GLFW.Key'S. keyUp GLFW.Key'W <|> 
		pure 0

--movement = (stopMoveAction . W.when (\(V2 x y) -> x == 0 && y == 0) <|> moveAction) . liftA2 (+) directionX directionY
movement :: InputWire a ()
movement = void (W.when (\(V2 dx dy) -> abs dx < 0.005 && abs dy < 0.005)) . liftA2 (+) directionX directionY
    W.--> moveAction . liftA2 (+) directionX directionY
    W.--> stopMoveAction W.--> movement

untilV :: (Monoid e, Monad m) => W.Wire s e m a (Event b) -> W.Wire s e m a ()
untilV source = W.until . fmap(\e -> ((), e)) source

--actionActivate :: InputWire a ()
--actionActivate = untilV (keyDownEvent GLFW.Key'X) W.-->
--	untilV (keyUpEvent GLFW.Key'X ). asSoonAs . keyDownEvent GLFW.Key'X
--	W.--> for 0.5 . asSoonAs . activateAction W.--> actionActivate

--actionPickup = asSoonAs . keyDownEvent GLFW.Key'C
spawn :: InputWire a ()
spawn = untilV (keyDownEvent GLFW.Key'Space)
	W.--> for 0.5 . asSoonAs . spawnAction . once . keyDownEvent GLFW.Key'Space 
	W.--> waitOneUpdate -- We need a state update at this point
	W.--> spawn

waitOneUpdate :: InputWire a ()
waitOneUpdate = mkGenN $ \_ ->
	return (Right (), inhibit ())

userInput :: InputWire a ((), ())
userInput = proc input -> do
	_ <- movement -< input
	_ <- spawn -< input
	returnA -< ((), ())

stopMoveAction :: InputWire a ()
stopMoveAction = mkGenN $ \_ -> do
	writer ((), newInputAction ActionStopMove)
	return (Right (), inhibit ())

moveAction :: InputWire (V2 Float) ()
moveAction = mkGenN $ \(V2 x y) ->
	if abs x < 0.005 && abs y < 0.005
        then
            return (Left (), moveAction)
        else do
            writer ((), newInputAction (newMoveAction x y))
            return (Right (), moveAction)

--activateAction :: InputWire a (Event ())
--activateAction = mkGenN $ \_ -> do
--	writer ((), newInputAction (ActionActivate DirNorth))
--	return (Right (Event ()), never)

spawnAction :: InputWire a (Event ())
spawnAction = mkGenN $ \_ -> do
	writer ((), newInputAction ActionSpawnArrow)
	return (Right (Event ()), never)
