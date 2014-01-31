
-- language pragmas to support: instance Monoid InputActions
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, Arrows #-}

module Game.Input.Input where

import Prelude hiding ((.))
import Control.Wire

-- for W.-->
import qualified Control.Wire as W 
import Control.Wire.Unsafe.Event

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.RWS
import Control.Monad

import qualified Graphics.UI.GLFW as GLFW

import Game.Input.Actions

import Linear

--data MouseEvent = 
--	  MouseClick
--	| MouseDoubleClick

-- for wires read only data
data UserInput = UserInput
  { inputKeys :: Set.Set GLFW.Key
  , inputMouseButtons :: Set.Set GLFW.MouseButton
  , inputMousePos :: (Double, Double)
  } deriving (Show)

data InputMemory = InputMemory
	{
	}

type InputContext = RWS UserInput InputActions InputMemory
type InputWire a b = Wire (Timed NominalDiffTime ()) () InputContext a b

inputNew = UserInput
  { inputKeys = Set.empty
  , inputMouseButtons = Set.empty
  , inputMousePos = (0, 0)
  }

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

inputEvent :: InputContext Bool -> InputWire a (Event a)
inputEvent cond = mkGenN $ \a -> do
  eventHappened <- cond
  return $ if eventHappened
    then (Right (Event a), inputEvent cond)
    else (Right NoEvent, inputEvent cond)

-- FIXME: merge with inputEvent?
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

directionX = pure (V2 1 0) . keyDown GLFW.Key'A . keyUp GLFW.Key'D <|> 
		pure (V2 (-1) 0) . keyDown GLFW.Key'D . keyUp GLFW.Key'A <|> 
		pure 0

directionY = pure (V2 0 (-1)) . keyDown GLFW.Key'W <|> 
		pure (V2 0 1) . keyDown GLFW.Key'S <|> 
		pure 0

movement = moveAction . liftA2 (+) directionX directionY

untilV source = W.until . fmap(\e -> ((), e)) source

actionActivate :: InputWire a ()
actionActivate = untilV (keyDownEvent GLFW.Key'X) W.-->
	untilV (keyUpEvent GLFW.Key'X ). asSoonAs . keyDownEvent GLFW.Key'X
	W.--> for 0.5 . asSoonAs . activateAction W.--> actionActivate

--actionPickup = asSoonAs . keyDownEvent GLFW.Key'C

userInput = proc input -> do
	m <- movement -< input
	a <- actionActivate -< input
	returnA -< (m, a)

moveAction = mkGenN $ \(V2 x y) -> do
	writer ((), newInputAction (newMoveAction x y))
	return (Right (), moveAction)

activateAction = mkGenN $ \_ -> do
	writer ((), newInputAction (ActionActivate DirNorth))
	return (Right (Event ()), never)

stepInput :: (Num a, Show b) =>
	InputWire a b ->
	Session IO (Timed NominalDiffTime ()) -> State UserInput () ->
	IO (InputActions, Session IO (Timed NominalDiffTime ()), InputWire a b)
stepInput w' session' input = do
	(dt, session) <- stepSession session'
	let ((_, w), _, actions) = runRWS (
		stepWire w' dt (Right 0)
		) (execState input inputNew) inputMemoryNew

	return (actions, session, w)
