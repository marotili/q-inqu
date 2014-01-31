{-# LANGUAGE NamedFieldPuns, Arrows #-}
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

import Game.Input.Actions as A

data DeltaAction = DeltaNew | DeltaDelete | DeltaUpdate

--type WorldContext = RWS World WorldDelta WorldManager
type DebugWorldContext = RWST World WorldDelta WorldManager IO
type WorldContext = DebugWorldContext

--type WorldWire a b = Wire (Timed NominalDiffTime ()) () WorldContext a b
type DebugWire a b = Wire (Timed NominalDiffTime ()) () DebugWorldContext a b
type WorldWire a b = DebugWire a b 
type WorldSession = Session IO (Timed NominalDiffTime ())

data WorldManager = WorldManager
	{ wmObjects :: ObjectIds 
	, wmObjectsInUse :: Map.Map ObjectId ObjectId -- Map objectInUse owningObject
 	, wmObjectRefs :: Map.Map ObjectId ObjectIds
	, wmNextObjectId :: ObjectId
	, wmPlayerActions :: Map.Map PlayerId InputActions
	} deriving (Show, Eq)

newWorldManager = WorldManager
	{ wmObjects = Set.empty
	, wmObjectsInUse = Map.empty
	, wmObjectRefs = Map.empty
	, wmNextObjectId = 1
	, wmPlayerActions = Map.empty
	}

movingDirectionR = mkGenN $ \playerId -> do
	wm <- get
	let playerActions = asks wmPlayerActions wm Map.! playerId
	let direction = A.movingDirection playerActions
	return (Right direction, movingDirectionR)


data WorldDelta = WorldDelta
	{ wdDoorsAdd :: [Door] -- absolute
	, wdPositionsDelta :: Map.Map ObjectId (Float, Float) -- relative
	, wdPlayerAdd :: [Player] -- absolute
	, wdDoorControllers :: [DoorController] -- absolute
	} deriving (Show, Eq)

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

moveObjectR :: WorldWire (ObjectId, (Float, Float)) ()
moveObjectR = mkGen $ \ds (oId, (vx, vy)) -> do
	let dt = realToFrac (dtime ds)
	deltaMoveObject oId (dt*vx, dt*vy)
	return (Right (), moveObjectR)

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

userSpeed = 100

testwire = proc input -> do
	playerId <- asSoonAs . spawnPlayer -< input
	(x, y) <- movingDirectionR -< playerId
	_ <- moveObjectR -< (playerId, (x*userSpeed, y*userSpeed))
	returnA -< ()

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
	} deriving (Show, Eq)

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



--type Session = Session ()

type ObjectIds = Set.Set ObjectId
type ObjectId = Int
type DoorId = ObjectId
type DoorControllerId = ObjectId
type SwitchId = ObjectId
type PlayerId = ObjectId

data Player = Player
	{ playerId :: ObjectId
	, playerName :: String
	} deriving (Show, Eq)

data Door = Door
	{ doorId :: DoorId
	, doorOpen :: Bool
	} deriving (Show, Eq)
--newDoor :: DoorId -> Door
--newDoor doorId = Door doorId False

data DoorController = DoorController
	{ doorControllerId :: DoorControllerId
	, dcTargetDoorId :: DoorId
	, dcTimeRunning :: Float
	, dcTimeNeedsToOpen :: Float
	, dcStarted :: Bool
	} deriving (Show, Eq)
--newDoorController :: DoorId -> DoorControllerId -> DoorController
--newDoorController doorId dcId = DoorController dcId doorId 0 1 False

data Switch = Switch
	{ switchId :: SwitchId
	, switchOn :: Bool
	} deriving (Show, Eq)
--newSwitch :: SwitchId -> Switch
--newSwitch swithcId = Switch swithcId False

instance B.Binary WorldDelta where
	put WorldDelta
	 	{ wdDoorsAdd
	 	, wdPositionsDelta
	 	, wdPlayerAdd
	 	, wdDoorControllers
		} = do
			B.put wdDoorsAdd
			B.put wdPositionsDelta
			B.put wdPlayerAdd
			B.put wdDoorControllers

	get = do
		a <- B.get
		b <- B.get
		c <- B.get
		d <- B.get

		return WorldDelta
			{ wdDoorsAdd = a
			, wdPositionsDelta = b
			, wdPlayerAdd = c
			, wdDoorControllers = d
			}

instance B.Binary World where
	put World 
		{ wDoors
		, wDoorControllers
		, wSwitches
		, wPositions
		, wPlayers
		} = do
			B.put wDoors
			B.put wDoorControllers
			B.put wSwitches
			B.put wPositions
			B.put wPlayers

	get = do
		doors <- B.get
		doorControllers <- B.get
		switches <- B.get
		positions <- B.get
		players <- B.get

		return World 
			{ wDoors = doors
			, wDoorControllers = doorControllers
			, wSwitches = switches
			, wPositions = positions
			, wPlayers = players
			}

instance B.Binary Player where
	put (Player {playerId, playerName}) = do
		B.put playerId
		B.put playerName
	get = do
		pId <- B.get
		pName <- B.get
		return (Player pId pName)

instance B.Binary Door where
	put (Door { doorId, doorOpen }) = B.put doorId >> B.put doorOpen
	get = do
		dId <- B.get
		dOpen <- B.get
		return Door { doorId = dId, doorOpen = dOpen}

instance B.Binary DoorController where
	put DoorController 
		{ doorControllerId
		, dcTargetDoorId
		, dcTimeRunning
		, dcTimeNeedsToOpen
		, dcStarted
		} = do
			B.put doorControllerId
			B.put dcTargetDoorId
			B.put dcTimeRunning
			B.put dcTimeNeedsToOpen
			B.put dcStarted

	get = do
		dcId <- B.get
		dcTDId <- B.get
		dcTR <- B.get
		dcTNTO <- B.get
		dcS <- B.get

		return DoorController 
			{ doorControllerId = dcId
			, dcTargetDoorId = dcTDId
			, dcTimeRunning = dcTR
			, dcTimeNeedsToOpen = dcTNTO
			, dcStarted = dcS
			}

instance B.Binary Switch where
	put Switch
		{ switchId
		, switchOn
		} = B.put switchId >> B.put switchOn

	get = do
		sId <- B.get
		sOn <- B.get
		return Switch { switchId = sId, switchOn = sOn }

