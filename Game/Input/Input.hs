{-# LANGUAGE NamedFieldPuns, ScopedTypeVariables, FlexibleContexts, NoMonomorphismRestriction,
	FlexibleInstances, Rank2Types, RankNTypes, TypeFamilies, GADTs, Arrows #-}
module Game.Input.Input where

import Control.Monad.Identity (Identity)
import Control.Monad.State hiding (when)
import Control.Wire
import FRP.Netwire
--import Control.Monoid
import qualified Control.Wire as W
import Prelude hiding ((.), until, when)
import Control.Monad.Reader hiding (when)
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set
import Data.Maybe
import Control.Arrow

import Control.Wire.Unsafe.Event

import qualified Graphics.UI.GLFW as GLFW
import Linear


-- TODO cleanup
integral2D ::
    (Fractional a, HasTime t s)
    => (V2 a)  -- ^ Integration constant (aka start value).
    -> Wire s e m (V2 a) (V2 a)
integral2D x' =
    mkPure func
        where
        	func ds dxy = 
		       	x' `seq` (Right x', integral2D (x' + (dxy * dt2)))
		       	where
		        	dt = realToFrac (dtime ds)
		        	--dt2 :: (Fractional a) => V2 a
		        	dt2 = pure dt

keyDownEvent :: GLFW.Key -> Wire s () (ReaderT Input IO) a (a, Event a)
keyDownEvent key = monadEvent (ask >>= return . Set.member key . inputKeys)

keyUpEvent :: GLFW.Key -> Wire s () (ReaderT Input IO) a (a, Event a)
keyUpEvent key = monadEvent (ask >>= return . not . Set.member key . inputKeys)

keyDown k = monadState (ask >>= return . Set.member k . inputKeys)
keyUp k = monadState (ask >>= return . not . Set.member k . inputKeys)

--instance Fractional (Float, Float) where
--instance Num (Float, Float) where
--	(+) (x, y) (x', y') = (x+x', y+y')
--	(*) (x, y)

camSpeed = 200

speedX :: (MonadReader Input m) => W.Wire s () m a (V2 Float)
speedX = pure (V2 (camSpeed) 0) . keyDown GLFW.Key'A . keyUp GLFW.Key'D <|> 
		pure (V2 (-camSpeed) 0) . keyDown GLFW.Key'D . keyUp GLFW.Key'A <|> 
		pure 0

speedY :: (MonadReader Input m) => W.Wire s () m a (V2 Float)
speedY = pure (V2 0 (-camSpeed)) . keyDown GLFW.Key'W <|> 
		pure (V2 0 camSpeed) . keyDown GLFW.Key'S <|> 
		pure 0

cameraPos :: (HasTime t s, MonadReader Input m) => W.Wire s () m a (V2 Float)
cameraPos = integral2D (V2 0 0). (liftA2 (+) speedX speedY)

monadEvent :: (Monad m) => (m Bool) -> Wire s e m a (a, Event a)
monadEvent cond = mkGenN $ \a -> do
  eventHappened <- cond
  return $ if eventHappened
    then (Right (a, Event a), monadEvent cond)
    else (Right (a, NoEvent), monadEvent cond)

monadState :: (Monad m) => (m Bool) -> Wire s () m a a
monadState cond = mkGenN $ \a -> do
  eventHappened <- cond
  return $ if eventHappened
    then (Right a, monadState cond)
    else (Left () , monadState cond)

mouseButtonDownEvent' mb = fmap (\(a, e) -> e) (mouseButtonDownEvent mb)
mouseButtonUpEvent' mb = fmap (\(a, e) -> e) (mouseButtonUpEvent mb)
mouseButtonDown mb = monadState (
	ask >>=
    	return . Set.member mb . inputMouseButtons
  )

mouseButtonDownEvent mb = monadEvent (
  ask >>=
    return . Set.member mb . inputMouseButtons
  )
mouseButtonUpEvent mb = monadEvent (
  ask >>=
    return . not . Set.member mb . inputMouseButtons
  )

--mousePosition :: 
mousePosition = mkGenN $ \_ -> do
  pos <- asks inputMousePos
  return (Right pos, mousePosition)

keyDownEvent' k = fmap (\a e -> e) (keyDownEvent k)
keyUpEvent' k = fmap (\a e -> e) (keyUpEvent k)

choice :: (Monad m) => Wire s e m a b -> Wire s e m a b ->
			Wire s e m a b -> Wire s e m a b ->
			Wire s e m a b

choice w1 w2 wn1 wn2 = WGen $ \ds mx' -> do
	(mx, w1') <- stepWire w1 ds mx'
	case mx of
		Left _ | Right _ <- mx' -> stepWire wn1 ds mx'
		_ ->  (do
			(mx2, w2') <- stepWire w2 ds mx'
			case mx2 of
				Left _ | Right _ <- mx' -> stepWire wn2 ds mx'
				_ ->
					case mx' of
						Left _ -> mx `seq` return (mx, choice w1' w2' wn1 wn2)
						Right _ -> Right mx `seq` return (mx, choice w1' w2' wn1 wn2)
						--(mx, mx2) `seq` return ((mx, mx2), choice w1 w2 wn1 wn2)
			)

voidEvent = (\_ -> Event ())
noEvent = never
untilVoidEvent (Event _) = ((), Event ())
untilVoidEvent (NoEvent) = ((), NoEvent)

untilVoid w = until . fmap untilVoidEvent w

isEvent (Event _) = True
isEvent _ = False

data MouseEvent = 
	  MouseClick
	| MouseDoubleClick

dropInput = fmap (\_ -> ()) W.id

--consecutiveEvents :: (Monoid e, MonadReader Input m, HasTime () s, Fractional b) => Wire s e m a b
doubleClick = noEvent . untilVoid mouseClick W.--> choice (noEvent . W.for 0.2) (anotherClick) (doubleClick) (
	when (isEvent) . once . fmap (\_ -> Event MouseDoubleClick) now W.--> doubleClick)
	where
		anotherClick = noEvent . untilVoid mouseClick

mouseClick = start W.-->
		filterTreshold . liftA2 (,) startPos pos 
		<|> noEvent . mouseButtonDown GLFW.MouseButton'1 
		W.--> mouseClick
	where
		start = noEvent . until . mouseButtonDownEvent GLFW.MouseButton'1

		-- remember first click position
		startPos = asSoonAs. once . mouseButtonDownEvent' GLFW.MouseButton'1 . mousePosition
		-- note: call onlyl after mousebuttondownevent. otherwise the returned position is (0,0)
		pos = asSoonAs . once . mouseButtonUpEvent' GLFW.MouseButton'1 . mousePosition

		filterTreshold = when (isEvent) . once . fmap (\_ -> Event MouseClick) W.id . when (isInTreshold) . fmap clickDistanceTreshold' W.id

		isInTreshold d = (d < 50)
		clickDistanceTreshold' ((x, y), (x', y')) = d
		    where
		    	d = sqrt((x' - x)**2 + (y' - y)**2)

produce = W.pure 9


inputEntry :: (Monoid s, HasTime b s, Num b, Fractional b) => Wire s () (ReaderT Input IO) a (Event MouseEvent)
--inputEntry = mouseClick
inputEntry = doubleClick <& mouseClick

instance Show (Event (Double, Double)) where
	show (Event (x, y)) = show x ++ " / " ++ show y
	show NoEvent = "no event"

instance Show (Event MouseEvent) where
	show (Event MouseClick) = "Event: Click"
	show (Event MouseDoubleClick) = "Event: Double click"
	show (NoEvent) = "No event"

instance Show (Event ((Double, Double), (Double, Double))) where
	show (Event ((x, y), (x', y'))) = show x ++ " / " ++ show y ++ "||" ++ show x' ++ " / " ++ show y'
	show NoEvent = "no event"

instance Show (Event ()) where
	show (Event _) = show "Event"
	show NoEvent = "No event"

instance Show (Event Double) where
	show (Event x) = show x
	show NoEvent = "No event"

data Move = Move deriving (Show)
type MoveEvent = Event NominalDiffTime --deriving (Show)

data Key = A | B deriving (Eq, Ord, Show)
data Input = Input
  { inputKeys :: Set.Set GLFW.Key
  , inputMouseButtons :: Set.Set GLFW.MouseButton
  , inputMousePos :: (Double, Double)
  } deriving (Show)

inputNew = Input
  { inputKeys = Set.empty
  , inputMouseButtons = Set.empty
  , inputMousePos = (0, 0)
  }

inputKeyDown input k = input { inputKeys = Set.insert k (inputKeys input) }
inputKeyUp input k = input { inputKeys = Set.delete k (inputKeys input) }

inputMouseButtonDown input mb = input { inputMouseButtons = Set.insert mb (inputMouseButtons input) }
inputMouseButtonUp input mb = input { inputMouseButtons = Set.delete mb (inputMouseButtons input) }

inputUpdateMousePos input pos = input { inputMousePos = pos }

step :: (Num a, Show b, Monoid e) =>
	Wire (Timed NominalDiffTime ()) e (ReaderT Input IO) a b ->
	Session IO (Timed NominalDiffTime ()) -> Input ->
	IO (Session IO (Timed NominalDiffTime ()), Wire (Timed NominalDiffTime ()) e (ReaderT Input IO) a b)
step w' session' input = do
	(dt, session) <- stepSession session'
	(mx, w) <- runReaderT (do
		stepWire w' dt (Right 0)
		) input

	--case mx of
	--	Left _ -> putStrLn ("Inhibited: ")
	--	Right x -> putStrLn ("Produced: " ++ show x)
	return (session, w)

stepGame :: (Num a, Show b, Monoid e) =>
	Wire (Timed NominalDiffTime ()) e (ReaderT Input IO) a b ->
	Session IO (Timed NominalDiffTime ()) -> Input ->
	IO (Either e b, Session IO (Timed NominalDiffTime ()), Wire (Timed NominalDiffTime ()) e (ReaderT Input IO) a b)
stepGame w' session' input = do
	(dt, session) <- stepSession session'
	(mx, w) <- runReaderT (do
		stepWire w' dt (Right 0)
		) input

	return (mx, session, w)
	--networkLoop w session

--performAttack ::(Monoid s) => Wire s e (Reader World) a AttackPower
--performAttack = mkGen $ \_ _ -> do
--		unit1' <- asks unit1
--		unit2' <- asks unit2
--		let pa = unitInstancePerformAttack unit1' unit2'
--		return (Right pa, pure pa)

--type ObjectId = Int
--data BaseDoor = BaseDoor
--	{ baseDoorId :: ObjectId
--	}

--data BaseDoorState = BaseDoorState
--	{ baseDoorIsOpening :: Bool
--	, baseDoorIsClosing :: Bool
--	, baseDoorOpen :: Bool
--	}

--data Triggerable where
--	Triggerable :: Triggerable_ a => a -> Triggerable

--instance Triggerable_ Triggerable where
--	isActive w (Trigger t) = isActive w t
--	activate w (Trigger t) = activate w t

--class Triggerable_ a where
--	isActive :: World -> a -> Bool
--	canBeActivated :: World -> a -> Bool
--	canBeReset :: World -> a -> Bool

--	activate :: World -> a -> World
--	reset :: World -> a -> World

--data Trigger = 
--	  BaseTrigger 
--	{ triggerId :: ObjectId 
--	}
--	| InputTrigger 
--	{ triggerId :: ObjectId 
--	}

--data BaseTriggerState = BaseTriggerState 
--	{ baseTriggerActive :: Bool
--	--, baseTriggerTarget :: Triggerable
--	}
--data InputTriggerState = InputTriggerState
--	{ inputTriggerActive :: Bool
--	, inputTriggerSource :: Triggerable
--	}

--data Controller = Controller
--	{ currentTime :: Float
--	, duration :: Float
--	}

----currentControl = from + (to - from)*(currentTime/duration)

--data Switch = Switch
--	{ switchState :: Bool
--	}

--data Door = Door
--	{ doorId :: Int
--	, doorOpen :: Bool -- open or closed
--	}

--data WorldDelta = WorldDelta
--	{ updatedDoors :: [Door]
--	}

--deltaDoor door = WorldDelta { updatedDoors = [door] }

--data DoorController =

--data Controllers = Controllers
--	{ doorControllers :: [(Door, Controller, Wire NominalDiffTime () IO Controller (Either Controller WorldDelta))]
--	}

--runControllers :: (Monoid s) => Wire s e m Controllers Controllers
--runControllers = mkPure runControllers_

--runControllers_ ds controllers = Right newControllers
--		where
--			newControllers = fmap (\(door, (control', w')) -> (door, control', w')) $ zip (fmap (\(a, b, c) -> a) doorControls) runWires
--			runWires = fmap (\(door, control, w) -> stepWire w ds control) doorControls
--			doorControls = doorControllers controllers



----mkDoorController Door -> Wire NominalDiffTime () IO Controller (Either Controller WorldDelta)
--mkDoorController door = (door, Controller 0 4, 
--		fmap Left controllerFor W.--> 
--		pure (Right . deltaDoor $ Door { doorId = doorId door, doorOpen = True })
--	)

-- door closed -> control -> door open

--doorIsOpen = mkPure $ \door -> if doorOpen door 
--			then (Right door, doorIsOpen) 
--			else (Left (), doorIsOpen)

--openDoor :: Wire s () m Door WorldDelta
--openDoor = doorIsOpen <|> (controller . fmap animControl W.id) W.--> 

--data World = World
--	{ switch1 :: Switch
--	, door1 :: Door
--	, controllers :: Controllers
--	}

---- runs until currentTime >= duration
--controllerFor :: (Monoid s, HasTime Float s, Monad m) => Wire s () m Controller Controller
--controllerFor = mkGen $ \s a -> do
--		let newCont = a { currentTime = (currentTime a + dtime s) }
--		return $ if currentTime newCont >= duration newCont then	
--			(Left (), controllerFor)
--		else
--			(Right newCont, controllerFor)


--instance Triggerable_ BaseTrigger where
--	isActive world trigger = (baseTriggerActive state)
--		where
--			state = (_worldBaseTriggerState world) Map.! (triggerId trigger)

--	activate world trigger = world { _worldBaseTriggerState = newState }
--		where
--			baseTriggerState = (_worldBaseTriggerState world) Map.! (triggerId trigger)
--			newState = Map.empty

--instance Triggerable_ InputTrigger where
--	isActive world trigger = inputActive && (inputTriggerActive state)
--		where
--			inputActive = isActive world (inputTriggerSource state)
--			state = (_worldInputTriggerState world) Map.! (triggerId trigger)

--data World = World
--	{ worldName :: String
--	--, worldDoors :: [Door]
--	, worldTriggers :: [Triggerable]
--	, _worldBaseDoorState :: Map.Map ObjectId BaseDoorState
--	, _worldBaseTriggerState :: Map.Map ObjectId BaseTriggerState
--	, _worldInputTriggerState :: Map.Map ObjectId InputTriggerState
--	}

--test = asSoonAs . once . Event ActivateTrigger

--mySession = clockSession <*> pure WorldDelta

--canTrigger :: Wire (Timed NominalDiffTime WorldDelta) 


-- delta states network synchronisation wires actions user input

-- World State: the current world synchronized with server
-- World delta state: world state + interpolation

-- wires should not store local data

		-- filterTreshold . liftA2 (,) startPos pos 
		-- <|> noEvent . mouseButtonDown GLFW.MouseButton'1 
		-- W.--> mouseClick

-- synchronized: other user actions, positions, states


-- not synchronized: user input

type ObjectId = Int
type DoorId = ObjectId
type DoorControllerId = ObjectId
type SwitchId = ObjectId

data Door = Door
	{ doorId :: DoorId
	, doorOpen :: Bool
	} deriving (Show)
newDoor :: DoorId -> Door
newDoor doorId = Door doorId False

data DoorController = DoorController
	{ doorControllerId :: DoorControllerId
	, dcTargetDoorId :: DoorId
	, dcTimeRunning :: NominalDiffTime
	, dcTimeNeedsToOpen :: NominalDiffTime
	, dcStarted :: Bool
	} deriving (Show)
newDoorController :: DoorId -> DoorControllerId -> DoorController
newDoorController doorId dcId = DoorController dcId doorId 0 1 False

data Switch = Switch
	{ switchId :: SwitchId
	, switchOn :: Bool
	} deriving (Show)
newSwitch :: SwitchId -> Switch
newSwitch swithcId = Switch swithcId False

type ObjectIds = Set.Set ObjectId

data World = World
	{ worldDoors :: Map.Map DoorId Door
	, worldDoorControllers :: Map.Map DoorControllerId DoorController
	, worldSwitches :: Map.Map SwitchId Switch

	, _objects :: ObjectIds
	, _objectsInUse :: Map.Map ObjectId ObjectId -- Map objectInUse owningObject
 	, _objectRefs :: Map.Map ObjectId ObjectIds
	, _nextObjectId :: ObjectId
	} deriving (Show)

newWorld = World
	{ worldDoors = Map.empty
	, worldDoorControllers = Map.empty
	, worldSwitches = Map.empty
	, _objects = Set.empty
	, _objectsInUse = Map.empty
	, _objectRefs = Map.empty
	, _nextObjectId = 1
	}  

-- TODO work with 'lens' to abstract setters

type WorldWire a b = Wire (Timed NominalDiffTime ()) () (State World) a b


lockObject = mkGenN $ \(ownerId, objectId) -> do
	s1 <- worldLockObject ownerId objectId
	return $ if s1 
		then (Right (ownerId, objectId), lockObject)
		else (Left (), lockObject)

unlockObject = mkGenN $ \(ownerId, objectId) -> do
	worldUnlockObject ownerId objectId
	return (Right (ownerId, objectId), unlockObject)

doorController = mkGenN $ \dcId -> do
	c <- worldDoorController dcId
	return (Right c, doorController)

updateDoorController = mkGenN $ \dc -> do
	modify $ \w -> w { worldDoorControllers = Map.insert (doorControllerId dc) dc (worldDoorControllers w) }
	return (Right (Event ()), updateDoorController)

switchDoor = mkGenN $ \dId -> do
	worldSwitchDoor dId
	return (Right (Event ()), switchDoor)

dTime = mkGen $ \ds _ -> return $ (Right (dtime ds), dTime)

runController :: DoorControllerId -> WorldWire a ()
runController dcId = proc input -> do
	controller <- doorController -< dcId
	let dId = dcTargetDoorId controller
	--lockObject W.--> inhibit () . unlockObject -< (dcId, dcId)
	--lockObject . fmap fst W.id W.--> inhibit () . fmap fst ((unlockObject . fmap fst W.id) &&& (unlockObject . fmap snd W.id)) -< ((dcId, dId), (dcId, dcId))
	dt <- dTime -< input
	let time = dcTimeRunning controller + dt
	if time >= dcTimeNeedsToOpen controller
		then do
			switchDoor -< dId
			inhibit () -< input
		else do
			let updatedController = controller 
				{ dcTimeRunning = time
				, dcStarted = True
				}		
			updateDoorController -< updatedController

	returnA -< ()

mkWorld :: State World (ObjectId, ObjectId)
mkWorld = do
	put newWorld
	door <- worldNewDoor
	dc <- worldNewDoorController door
	return (doorId door, doorControllerId dc)

testMain = do
	let ((dId, dcId), world) = (runState mkWorld) newWorld
	print (dId, dcId)

	let session' = clockSession_
	loop session' world dcId

	where loop session' world' dcId = do
			(dt, session) <- stepSession session'
			print dt
			let (w, world) = runState (do 
				stepWire (runController dcId) dt (Right ())
				) world'
			print $ fst w
			print (worldDoors world)
			loop session world dcId


worldNewSwitch :: State World Switch
worldNewSwitch = do
	(switch, newSwitches) <- worldNewObject worldSwitches newSwitch
	modify $ \w -> w { worldSwitches = newSwitches }
	return switch

worldDoorController :: DoorControllerId -> State World DoorController
worldDoorController dcId = do
	w <- get
	return $ (worldDoorControllers w) Map.! dcId

worldNewDoorController :: Door -> State World DoorController
worldNewDoorController Door { doorId } = do
	(doorController, newDoorControllers) <- worldNewObject worldDoorControllers (newDoorController doorId)
	_worldAddObjectRef (doorControllerId doorController) doorId
	modify $ \w -> w { worldDoorControllers = newDoorControllers }
	return doorController

worldDoor :: DoorId -> State World Door
worldDoor dId = get >>= \w -> return $ (worldDoors w) Map.! dId

worldSwitchDoor :: DoorId -> State World ()
worldSwitchDoor dId = do
	targetDoor <- worldDoor dId
	let updatedDoor = targetDoor { doorOpen = not (doorOpen targetDoor) }
	modify $ \w -> w { worldDoors = Map.insert dId updatedDoor (worldDoors w) }

worldNewDoor :: State World Door
worldNewDoor = do
	(door, newWorldDoors) <- worldNewObject worldDoors newDoor
	modify $ \w -> w { worldDoors = newWorldDoors }
	return door

worldNewObject :: (World -> Map.Map ObjectId a) -> (ObjectId -> a) -> State World (a, Map.Map ObjectId a)
worldNewObject getter constructor = do
	w <- get
	newId <- _worldGenObjectId
	let obj = constructor newId
	return $ (obj, Map.insert newId obj (getter w))

_worldAddObject :: ObjectId -> State World ()
_worldAddObject objectId = do
	modify $ \w -> w { _objects = Set.insert objectId (_objects w) }

_worldGenObjectId :: State World ObjectId
_worldGenObjectId = do
	world <- get
	let objectId = _nextObjectId world
	modify $ \w -> w { _nextObjectId = objectId + 1 }
	_worldAddObject objectId
	return objectId

-- TODO: reason about circular dependencies
_worldAddObjectRef :: ObjectId -> ObjectId -> State World ()
_worldAddObjectRef objectId targetObjectId = do
	modify $ \w -> w { _objectRefs = newObjectRefs w }
	where
		newObjectRefs world = Map.alter alterRef objectId (_objectRefs world)
		alterRef Nothing = Just $ Set.insert targetObjectId Set.empty
		alterRef (Just ids) = Just $ Set.insert targetObjectId ids

worldObjectLocked :: ObjectId -> State World Bool
worldObjectLocked oId = get >>= (\w -> return $ if Map.member oId (_objectsInUse w) then True else False)

-- lock an object, returns updated world state on success, nothing on failure
worldLockObject :: ObjectId -> ObjectId -> State World Bool
worldLockObject ownerId objectId = do
	world <- get
	if objectInUse world || objectExists world
		then return False 
		else do
			modify $ \world -> world { _objectsInUse = Map.insert objectId ownerId (_objectsInUse world)}
			return True
	where
		objectInUse world = Map.member objectId (_objectsInUse world)
		objectExists world = Set.member objectId (_objects world)

worldUnlockObject :: ObjectId -> ObjectId -> State World ()
worldUnlockObject ownerId objectId = do
	w <- get
	if (_objectsInUse w) Map.! objectId == ownerId -- only unlock if owner of object
		then modify $ \w -> w { _objectsInUse = Map.delete objectId (_objectsInUse w) }
		else return ()
