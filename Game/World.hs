{-# LANGUAGE NamedFieldPuns #-}
module Game.World where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad

import qualified Data.Binary as B

import Control.Wire
import qualified Control.Wire as W
import Control.Wire.Unsafe.Event
import qualified Prelude as P
import Prelude hiding ((.), until)

data DeltaAction = DeltaNew | DeltaDelete | DeltaUpdate

data WorldDelta = WorldDelta
	{ wdDoorsAdd :: [Door] -- absolute
	, wdPositionsDelta :: Map.Map ObjectId (Float, Float) -- relative
	, wdPlayerAdd :: [Player] -- absolute
	, wdDoorControllers :: [DoorController] -- absolute
	} deriving (Show)

deltaMoveObject :: ObjectId -> (Float, Float) -> WorldContext ()
deltaMoveObject oId dPos@(dx, dy) = writer ((), emptyDelta
	{ wdPositionsDelta = Map.insert oId dPos Map.empty
	})

applyDelta :: World -> WorldDelta -> World
applyDelta w wd = doorControllers
	where
		newDoors = w { wDoors = foldr (\d -> Map.insert (doorId d) d) (wDoors w) (wdDoorsAdd wd) }
		newPlayers = newDoors { wPlayers = foldr (\p -> Map.insert (playerId p) p) (wPlayers newDoors) (wdPlayerAdd wd) }
		positions = newPlayers { wPositions = foldr (\(k, v) m -> Map.alter (alterPos v) k m) (wPositions newPlayers) (Map.toList . wdPositionsDelta $ wd) }
		doorControllers = positions { wDoorControllers = foldr (\dc -> Map.insert (doorControllerId dc) dc) (wDoorControllers positions) (wdDoorControllers wd) }

--validateWorldDelta :: WorldDelta -> Bool

-- TODO
-- fix collisions (reset positions)
--fixWorldDelta :: World -> WorldDelta -> WorldDelta

alterPos val Nothing = Just val
alterPos (x, y) (Just (x', y')) = Just (x + x', y + y')

emptyDelta = WorldDelta 
	{ wdDoorsAdd = []
	, wdPositionsDelta = Map.empty
	, wdPlayerAdd = []
	, wdDoorControllers = []
	}

instance Monoid WorldDelta where
	mempty = emptyDelta
	mappend wd1 wd2 = WorldDelta
		{ wdDoorsAdd = doors
		, wdPositionsDelta = positions
		, wdPlayerAdd = players
		, wdDoorControllers = doorControllers
		}
		where
			doors = wdDoorsAdd wd1 ++ wdDoorsAdd wd2
			players = wdPlayerAdd wd1 ++ wdPlayerAdd wd2
			positions = foldr (\(k, v) m -> Map.alter (alterPos v) k m) (wdPositionsDelta wd1) (Map.toList . wdPositionsDelta $ wd2)
			doorControllers = wdDoorControllers wd1 ++ wdDoorControllers wd2

data WorldManager = WorldManager
	{ wmObjects :: ObjectIds 
	, wmObjectsInUse :: Map.Map ObjectId ObjectId -- Map objectInUse owningObject
 	, wmObjectRefs :: Map.Map ObjectId ObjectIds
	, wmNextObjectId :: ObjectId
	} deriving (Show)

newWorldManager = WorldManager
	{ wmObjects = Set.empty
	, wmObjectsInUse = Map.empty
	, wmObjectRefs = Map.empty
	, wmNextObjectId = 1
	}

deltaAddDoor :: (Float, Float) -> ObjectId -> WorldContext ()
deltaAddDoor pos oId = do
	deltaMoveObject oId pos
	writer ((), mempty
		{ wdDoorsAdd = [door]
		})
	where
		door = Door oId False

deltaAddDoorController :: DoorId -> DoorControllerId -> WorldContext ()
deltaAddDoorController doorId oId = 
	writer ((), mempty
		{ wdDoorControllers = [doorController]
		})
	where
		doorController = DoorController oId doorId 0 2 False

deltaUpdateDoorController :: DoorController -> WorldContext ()
deltaUpdateDoorController dc =
	writer ((), mempty
		{ wdDoorControllers = [dc]
		})

deltaAddPlayer :: String -> (Float, Float) -> ObjectId -> WorldContext ()
deltaAddPlayer name pos oId = do
	deltaMoveObject oId pos
	writer ((), mempty 
		{ wdPlayerAdd = [player]
		})
	where
		player = Player oId name

newObject :: WorldContext ObjectId
newObject = do
	wm <- get 
	let oId = wmNextObjectId wm
	modify $ \wm -> wm 
		{ wmNextObjectId = oId + 1
		, wmObjects = Set.insert oId (wmObjects wm)
		}
	return oId

moveObject :: (Float, Float) -> WorldWire ObjectId ()
moveObject (vx, vy) = mkGen $ \ds oId -> do
	let dt = realToFrac (dtime ds)
	deltaMoveObject oId (dt*vx, dt*vy)
	return (Right (), moveObject (vx, vy))

--wcSpawnDoor :: WorldContext ()
--wcSpawnPlayer = newObject >>= deltaAddDoor

--wcSpawnPlayer :: WorldContext ()
--wcSpawnPlayer = newObject >>= deltaAddPlayer "Marco" (50, 50)
spawnPlayer :: WorldWire a (Event PlayerId)
spawnPlayer = mkObjGenWire $ deltaAddPlayer "Marco" (50, 50)

spawnDoor :: WorldWire a (Event DoorId)
spawnDoor = mkObjGenWire $ deltaAddDoor (20, 20)

nullObject :: ObjectId
nullObject = 0

genLater :: WorldWire (Event ObjectId) (Event ObjectId)
--genLater = delay (Event nullObject)
genLater = delay NoEvent

spawnDoorController :: DoorId -> WorldWire a (Event DoorControllerId)
spawnDoorController dId = mkObjGenWire $ deltaAddDoorController dId

data ControllerState = Started | Finished

runController :: WorldWire DoorControllerId (Event ControllerState)
runController = mkGen $ \ds dcId -> do
	dc <- doorController dcId 
	let time' = dcTimeRunning dc + realToFrac (dtime ds)
	if time' > dcTimeNeedsToOpen dc
		then do
			deltaUpdateDoorController $ dc 
				{ dcTimeRunning = 0
				, dcStarted = False
				}
			return (Right $ Event Finished, never)
		else do
			deltaUpdateDoorController $ dc 
				{ dcTimeRunning = time'
				, dcStarted = True
				}
			return (Right NoEvent, runController)

openDoor = until . fmap (\e -> ((), e)) runController

mkObjGenWire :: (ObjectId -> WorldContext ()) -> WorldWire a (Event ObjectId)
mkObjGenWire f = genLater . mkGenN (\_ -> do
		oId <- newObject
		f oId
		return (Right (Event oId), never)
	)

testwire = runController . asSoonAs . spawnDoorController 1 . asSoonAs . spawnDoor

--makeWorld = WorldContext ()
--makeWorld = spawnPlayer

worldLoop w' session' world' state' = do
	(dt, session) <- stepSession session'
	((out, w), worldManager, worldDelta) <- runRWST (
		stepWire w' dt (Right ())
		) world' state'

	let quit = case out of
		Right _ -> False
		Left _ -> True
	return (quit, (w, session), (worldManager, worldDelta))

testWorld :: IO ()
testWorld = do
	let session = clockSession_
	let world = newWorld

	loop world newWorldManager testwire session

	where loop world manager w session = do
		(quit, (w', session'), (manager', delta)) <- worldLoop w session world manager
		let world' = applyDelta world delta
		print (world', quit)
		let quit = False
		Control.Monad.unless quit $ loop world' manager' w' session'

data World = World
	{ wDoors :: Map.Map DoorId Door
	, wDoorControllers :: Map.Map DoorControllerId DoorController
	, wSwitches :: Map.Map SwitchId Switch
	, wPositions :: Map.Map ObjectId (Float, Float)
	, wPlayers :: Map.Map ObjectId Player
	} deriving (Show)

newWorld = World
	{ wDoors = Map.empty
	, wDoorControllers = Map.empty
	, wSwitches = Map.empty
	, wPositions = Map.empty
	, wPlayers = Map.empty
	}

doorController :: DoorControllerId -> WorldContext DoorController
doorController oId = do
	liftIO $ print oId
	dcs <- asks wDoorControllers
	liftIO $ print dcs
	return $ dcs Map.! oId

--type WorldContext = RWS World WorldDelta WorldManager
type DebugWorldContext = RWST World WorldDelta WorldManager IO
type WorldContext = DebugWorldContext

--type WorldWire a b = Wire (Timed NominalDiffTime ()) () WorldContext a b
type DebugWire a b = Wire (Timed NominalDiffTime ()) () DebugWorldContext a b
type WorldWire a b = DebugWire a b 

type ObjectIds = Set.Set ObjectId
type ObjectId = Int
type DoorId = ObjectId
type DoorControllerId = ObjectId
type SwitchId = ObjectId
type PlayerId = ObjectId

data Player = Player
	{ playerId :: ObjectId
	, playerName :: String
	} deriving (Show)

instance B.Binary Player where
	put (Player {playerId, playerName}) = do
		B.put playerId
		B.put playerName
	get = do
		pId <- B.get
		pName <- B.get
		return (Player pId pName)

data Door = Door
	{ doorId :: DoorId
	, doorOpen :: Bool
	} deriving (Show)
--newDoor :: DoorId -> Door
--newDoor doorId = Door doorId False

data DoorController = DoorController
	{ doorControllerId :: DoorControllerId
	, dcTargetDoorId :: DoorId
	, dcTimeRunning :: NominalDiffTime
	, dcTimeNeedsToOpen :: NominalDiffTime
	, dcStarted :: Bool
	} deriving (Show)
--newDoorController :: DoorId -> DoorControllerId -> DoorController
--newDoorController doorId dcId = DoorController dcId doorId 0 1 False

data Switch = Switch
	{ switchId :: SwitchId
	, switchOn :: Bool
	} deriving (Show)
--newSwitch :: SwitchId -> Switch
--newSwitch swithcId = Switch swithcId False



--newWorld = World
--	{ worldDoors = Map.empty
--	, worldDoorControllers = Map.empty
--	, worldSwitches = Map.empty
--	, _objects = Set.empty
--	, _objectsInUse = Map.empty
--	, _objectRefs = Map.empty
--	, _nextObjectId = 1
--	}  

---- TODO work with 'lens' to abstract setters

--type WorldWire a b = Wire (Timed NominalDiffTime ()) () (State World) a b


--lockObject = mkGenN $ \(ownerId, objectId) -> do
--	s1 <- worldLockObject ownerId objectId
--	return $ if s1 
--		then (Right (ownerId, objectId), lockObject)
--		else (Left (), lockObject)

--unlockObject = mkGenN $ \(ownerId, objectId) -> do
--	worldUnlockObject ownerId objectId
--	return (Right (ownerId, objectId), unlockObject)

--doorController = mkGenN $ \dcId -> do
--	c <- worldDoorController dcId
--	return (Right c, doorController)

--updateDoorController = mkGenN $ \dc -> do
--	modify $ \w -> w { worldDoorControllers = Map.insert (doorControllerId dc) dc (worldDoorControllers w) }
--	return (Right (Event ()), updateDoorController)

--switchDoor = mkGenN $ \dId -> do
--	worldSwitchDoor dId
--	return (Right (Event ()), switchDoor)

--dTime = mkGen $ \ds _ -> return $ (Right (dtime ds), dTime)

--runController :: DoorControllerId -> WorldWire a ()
--runController dcId = proc input -> do
--	controller <- doorController -< dcId
--	let dId = dcTargetDoorId controller
--	--lockObject W.--> inhibit () . unlockObject -< (dcId, dcId)
--	--lockObject . fmap fst W.id W.--> inhibit () . fmap fst ((unlockObject . fmap fst W.id) &&& (unlockObject . fmap snd W.id)) -< ((dcId, dId), (dcId, dcId))
--	dt <- dTime -< input
--	let time = dcTimeRunning controller + dt
--	if time >= dcTimeNeedsToOpen controller
--		then do
--			switchDoor -< dId
--			inhibit () -< input
--		else do
--			let updatedController = controller 
--				{ dcTimeRunning = time
--				, dcStarted = True
--				}		
--			updateDoorController -< updatedController

--	returnA -< ()


--mkWorld :: State World (ObjectId, ObjectId)
--mkWorld = do
--	put newWorld
--	door <- worldNewDoor
--	dc <- worldNewDoorController door
--	return (doorId door, doorControllerId dc)

--testMain = do
--	let ((dId, dcId), world) = (runState mkWorld) newWorld
--	print (dId, dcId)

--	let session' = clockSession_
--	loop session' world dcId

--	where loop session' world' dcId = do
--			(dt, session) <- stepSession session'
--			print dt
--			let (w, world) = runState (do 
--				stepWire (runController dcId) dt (Right ())
--				) world'
--			print $ fst w
--			print (worldDoors world)
--			loop session world dcId

-- loop: worldContext w -> worldContext' w' -> worldContext'' w''	


--worldNewSwitch :: State World Switch
--worldNewSwitch = do
--	(switch, newSwitches) <- worldNewObject worldSwitches newSwitch
--	modify $ \w -> w { worldSwitches = newSwitches }
--	return switch

--worldDoorController :: DoorControllerId -> State World DoorController
--worldDoorController dcId = do
--	w <- get
--	return $ (worldDoorControllers w) Map.! dcId

--worldNewDoorController :: Door -> State World DoorController
--worldNewDoorController Door { doorId } = do
--	(doorController, newDoorControllers) <- worldNewObject worldDoorControllers (newDoorController doorId)
--	_worldAddObjectRef (doorControllerId doorController) doorId
--	modify $ \w -> w { worldDoorControllers = newDoorControllers }
--	return doorController

--worldDoor :: DoorId -> State World Door
--worldDoor dId = get >>= \w -> return $ (worldDoors w) Map.! dId

--worldSwitchDoor :: DoorId -> State World ()
--worldSwitchDoor dId = do
--	targetDoor <- worldDoor dId
--	let updatedDoor = targetDoor { doorOpen = not (doorOpen targetDoor) }
--	modify $ \w -> w { worldDoors = Map.insert dId updatedDoor (worldDoors w) }

--worldNewDoor :: State World Door
--worldNewDoor = do
--	(door, newWorldDoors) <- worldNewObject worldDoors newDoor
--	modify $ \w -> w { worldDoors = newWorldDoors }
--	return door

--worldNewObject :: (World -> Map.Map ObjectId a) -> (ObjectId -> a) -> State World (a, Map.Map ObjectId a)
--worldNewObject getter constructor = do
--	w <- get
--	newId <- _worldGenObjectId
--	let obj = constructor newId
--	return $ (obj, Map.insert newId obj (getter w))

--_worldAddObject :: ObjectId -> State World ()
--_worldAddObject objectId = do
--	modify $ \w -> w { _objects = Set.insert objectId (_objects w) }

--_worldGenObjectId :: State World ObjectId
--_worldGenObjectId = do
--	world <- get
--	let objectId = _nextObjectId world
--	modify $ \w -> w { _nextObjectId = objectId + 1 }
--	_worldAddObject objectId
--	return objectId

---- TODO: reason about circular dependencies
--_worldAddObjectRef :: ObjectId -> ObjectId -> State World ()
--_worldAddObjectRef objectId targetObjectId = do
--	modify $ \w -> w { _objectRefs = newObjectRefs w }
--	where
--		newObjectRefs world = Map.alter alterRef objectId (_objectRefs world)
--		alterRef Nothing = Just $ Set.insert targetObjectId Set.empty
--		alterRef (Just ids) = Just $ Set.insert targetObjectId ids

--worldObjectLocked :: ObjectId -> State World Bool
--worldObjectLocked oId = get >>= (\w -> return $ if Map.member oId (_objectsInUse w) then True else False)

---- lock an object, returns updated world state on success, nothing on failure
--worldLockObject :: ObjectId -> ObjectId -> State World Bool
--worldLockObject ownerId objectId = do
--	world <- get
--	if objectInUse world || objectExists world
--		then return False 
--		else do
--			modify $ \world -> world { _objectsInUse = Map.insert objectId ownerId (_objectsInUse world)}
--			return True
--	where
--		objectInUse world = Map.member objectId (_objectsInUse world)
--		objectExists world = Set.member objectId (_objects world)

--worldUnlockObject :: ObjectId -> ObjectId -> State World ()
--worldUnlockObject ownerId objectId = do
--	w <- get
--	if (_objectsInUse w) Map.! objectId == ownerId -- only unlock if owner of object
--		then modify $ \w -> w { _objectsInUse = Map.delete objectId (_objectsInUse w) }
--		else return ()
