{-# LANGUAGE TemplateHaskell, Arrows, NamedFieldPuns, Rank2Types #-}
module Game.World where

import Control.Concurrent

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

import Control.Lens
import Game.Objects
import Game.Collision

import Data.Tiled
import Game.Tiled

import Game.Input.Actions as A

data WorldDelta = WorldDelta
	{ _wdDoorsAdd :: [Door] -- absolute
	, _wdWallsAdd :: [Wall] -- absolute
	, _wdPositionsDelta :: Map.Map ObjectId (Float, Float) -- relative
	, _wdPlayerAdd :: [Player] -- absolute
	, _wdDoorControllers :: [DoorController] -- absolute
	, _wdCollisions :: Map.Map ObjectId [ObjectId]
	} deriving (Show, Eq)

makeLenses ''WorldDelta

data WorldManager = WorldManager
	{ _wmObjects :: ObjectIds 
	, _wmObjectsInUse :: Map.Map ObjectId ObjectId -- Map objectInUse owningObject
 	, _wmObjectRefs :: Map.Map ObjectId ObjectIds
	, _wmNextObjectId :: ObjectId
	, _wmPlayerActions :: Map.Map PlayerId InputActions
	} deriving (Show, Eq)

makeLenses ''WorldManager

data World = World
	{ _wDoors :: Map.Map DoorId Door
	, _wWalls :: Map.Map WallId Wall
	, _wDoorControllers :: Map.Map DoorControllerId DoorController
	, _wSwitches :: Map.Map SwitchId Switch
	, _wPositions :: Map.Map ObjectId (Float, Float)
	, _wPlayers :: Map.Map ObjectId Player
	, _wCollisionManager :: CollisionManager

	, _wCurrentCollisions :: Map.Map ObjectId [ObjectId]
	} deriving (Eq)

makeLenses ''World

--type WorldContext = RWS World WorldDelta WorldManager
type DebugWorldContext = RWST World WorldDelta WorldManager IO
type WorldContext = DebugWorldContext

--type WorldWire a b = Wire (Timed NominalDiffTime ()) () WorldContext a b
type DebugWire a b = Wire (Timed NominalDiffTime ()) () DebugWorldContext a b
type WorldWire a b = DebugWire a b 
type WorldSession = Session IO (Timed NominalDiffTime ())

newWorldManager = WorldManager
	{ _wmObjects = Set.empty
	, _wmObjectsInUse = Map.empty
	, _wmObjectRefs = Map.empty
	, _wmNextObjectId = 1
	, _wmPlayerActions = Map.empty
	}

newWorld = World
	{ _wDoors = Map.empty
	, _wWalls = Map.empty
	, _wDoorControllers = Map.empty
	, _wSwitches = Map.empty
	, _wPositions = Map.empty
	, _wPlayers = Map.empty
	, _wCollisionManager = cmNew
	, _wCurrentCollisions = Map.empty
	--, wMap = gameMap
	}

getPlayerId :: String -> Getter World (Maybe PlayerId)
getPlayerId playerName' = to (\w -> case getPlayer w of
			[pId] -> Just pId
			_ -> Nothing
		)
	where
		getPlayer world = ifoldMap (\pId Player { playerName } ->
				if playerName == playerName' then [pId] else []
			) (world^.wPlayers)

newWorldFromTiled :: TiledMap -> IO (World, WorldManager) -- io due to debug wire
newWorldFromTiled tiledMap = do
	let world = newWorld
	(worldManager, worldDelta) <- execRWST (do
			stepWire initWire (Timed 0 ()) (Right ())
		) world newWorldManager

	let world' = applyDelta world worldDelta
	return (world', worldManager)

	where
		Just player1Obj = queryObject tiledMap "Player1"
		Just player2Obj = queryObject tiledMap "Player2"

		wallPositions = mapWallPositions tiledMap

		--genWalls :: [(Float, Float)] -> WorldWire a b
		genWalls [] = returnA
		genWalls (wallPos:walls) = proc input -> do
			spawnWallAt wallPos -< input
			genWalls walls -< input
			returnA -< input

		initWire = proc input -> do
			_ <- spawnPlayerAt "Neira" (player1Obj^.objectPos) -< input
			_ <- spawnPlayerAt "TheGhost" (player2Obj^.objectPos) -< input

			_ <- genWalls wallPositions -< input

			returnA -< ()

movingDirectionR = mkGenN $ \playerId -> do
	wm <- get
	if Map.member playerId (wm^.wmPlayerActions) 
		then do
			let playerActions = asks _wmPlayerActions wm Map.! playerId
			let direction = A.movingDirection playerActions
			return (Right direction, movingDirectionR)
		else
			return (Right (0, 0), movingDirectionR)

deltaMoveObject :: ObjectId -> (Float, Float) -> WorldContext ()
deltaMoveObject oId dPos@(dx, dy) = do
	cm <- view wCollisionManager
	if cmCanCollide oId cm 
		then do
			-- FIXME: right now objects can jump through other objects if delta is too big
			let ((dx', dy'), collisions) = collisionLoop cm (dx, dy) []
			writer ((), newDelta
				{ _wdPositionsDelta = Map.insert oId (dx', dy') Map.empty
				})
			Control.Monad.unless (null collisions) $ writer ((), newDelta
				{ _wdCollisions = Map.insert oId collisions Map.empty
				})
		else
			writer ((), newDelta
				{ _wdPositionsDelta = Map.insert oId dPos Map.empty
				})
	where
		-- on collision we update the position so that the objects dont intersect
		-- and we send an collision event

		-- big delta -> maybe many collisions
		-- we reduce the delta pos and obtain less collisions. we return the minimum number of collisions possible
		collisionLoop :: CollisionManager -> (Float, Float) -> [ObjectId] -> ((Float, Float), [ObjectId])
		collisionLoop cm (dx, dy) collisionHappened = 		
			if null collisions then
				((dx, dy), collisionHappened) -- the last collision that happened
			else
				collisionLoop cm (dx/2.0, dy/2.0) collisions
			where
				collisions = evalState (do
						(px, py) <- cmObjectPos oId
						cmRemoveFloating oId 
						let bSize = 5
						let newPos = (px + dx, py + dy)
						cmAddFloating $ newCollidable oId (newBoundary newPos bSize)
						cmCollisions oId
					) cm



applyDelta :: World -> WorldDelta -> World
applyDelta w wd = collisions
	where
		newWalls = w { _wWalls = foldr (\d -> Map.insert (wallId d) d) (_wWalls w) (_wdWallsAdd wd) }
		newPlayers = newWalls { _wPlayers = foldr (\p -> Map.insert (playerId p) p) (_wPlayers newWalls) (_wdPlayerAdd wd) }
		positions = newPlayers { _wPositions = foldr (\(k, v) m -> Map.alter (alterPos v) k m) (_wPositions newPlayers) (Map.toList . _wdPositionsDelta $ wd) }
		collidables = positions { _wCollisionManager = execState (do
				mapM_ cmAddStatic [newCollidable oId (newBoundary (objectPos oId) 5) | oId <- map wallId (_wdWallsAdd wd)]
				mapM_ cmAddFloating [newCollidable oId (newBoundary (objectPos oId) 5) | oId <- map playerId (_wdPlayerAdd wd)]
			) (_wCollisionManager positions) }
		floatingCollidables = collidables { _wCollisionManager = execState (
				mapM_ (\oId -> cmUpdateFloating oId (objectPos oId)) (map fst (Map.toList (wd^.wdPositionsDelta)))
			) (_wCollisionManager collidables) }

		-- overwrite old ones
		collisions = floatingCollidables { _wCurrentCollisions = _wdCollisions wd }

		objectPos oId = (positions ^. wPositions) Map.! oId
		--doorControllers = positions { wDoorControllers = foldr (\dc -> Map.insert (doorControllerId dc) dc) (wDoorControllers positions) (wdDoorControllers wd) }

--validateWorldDelta :: WorldDelta -> Bool

-- TODO
-- fix collisions (reset positions)
--fixWorldDelta :: World -> WorldDelta -> WorldDelta

alterPos val Nothing = Just val
alterPos (x, y) (Just (x', y')) = Just (x + x', y + y')

newDelta = WorldDelta 
	{ _wdDoorsAdd = []
	, _wdWallsAdd = []
	, _wdPositionsDelta = Map.empty
	, _wdPlayerAdd = []
	, _wdDoorControllers = []
	, _wdCollisions = Map.empty
	}

instance Monoid WorldDelta where
	mempty = newDelta
	mappend wd1 wd2 = newDelta
		{ _wdDoorsAdd = doors
		, _wdWallsAdd = walls
		, _wdPositionsDelta = positions
		, _wdPlayerAdd = players
		, _wdDoorControllers = doorControllers
		, _wdCollisions = collisions
		}
		where
			walls = wd1^.wdWallsAdd ++ wd2^.wdWallsAdd
			doors = wd1^.wdDoorsAdd ++ wd2^.wdDoorsAdd
			players = wd1^.wdPlayerAdd ++ wd2^.wdPlayerAdd
			positions = foldr (\(k, v) m -> Map.alter (alterPos v) k m) (wd1^.wdPositionsDelta) (Map.toList (wd2^.wdPositionsDelta))
			doorControllers = wd1^.wdDoorControllers ++ wd2^.wdDoorControllers
			collisions = Map.unionWith (++) (wd1^.wdCollisions) (wd2^.wdCollisions)


--deltaAddDoor :: (Float, Float) -> ObjectId -> WorldContext ()
--deltaAddDoor pos oId = do
--	deltaMoveObject oId pos
--	writer ((), mempty
--		{ wdDoorsAdd = [door]
--		})
--	where
--		door = Door oId False

--deltaAddDoorController :: DoorId -> DoorControllerId -> WorldContext ()
--deltaAddDoorController doorId oId = 
--	writer ((), mempty
--		{ wdDoorControllers = [doorController]
--		})
--	where
--		doorController = DoorController oId doorId 0 2 False

--deltaUpdateDoorController :: DoorController -> WorldContext ()
--deltaUpdateDoorController dc =
--	writer ((), mempty
--		{ wdDoorControllers = [dc]
--		})

deltaAddPlayer :: String -> (Float, Float) -> ObjectId -> WorldContext ()
deltaAddPlayer name pos oId = do
	deltaMoveObject oId pos
	writer ((), mempty 
		{ _wdPlayerAdd = [player]
		})
	where
		player = Player oId name

deltaAddWall :: (Float, Float) -> ObjectId -> WorldContext ()
deltaAddWall pos oId = do
	deltaMoveObject oId pos
	writer ((), mempty 
		{ _wdWallsAdd = [Wall oId]
		})

newObject :: WorldContext ObjectId
newObject = do
	wm <- get 
	let oId = (wm^.wmNextObjectId)
	wmNextObjectId += 1
	wmObjects .= Set.insert oId (wm^.wmObjects)
	--modify $ \wm -> wm 
		--{ _wmNextObjectId = oId + 1
		--, _wmObjects = Set.insert oId (wm^.wmObjects)
		--}
	return oId

moveObject :: (Float, Float) -> WorldWire ObjectId ()
moveObject (vx, vy) = mkGen $ \ds oId -> do
	let dt = realToFrac (dtime ds)
	deltaMoveObject oId (dt*vx, dt*vy)
	return (Right (), moveObject (vx, vy))

objectCollided :: WorldWire ObjectId (Event ())
objectCollided = mkGenN $ \oId -> do
	w <- ask
	let currentColls = w^.wCurrentCollisions
	return $ if Map.member oId currentColls
		then (Right (Event ()), objectCollided)
		else (Right NoEvent, objectCollided)

moveObjectR :: WorldWire (ObjectId, (Float, Float)) ()
moveObjectR = mkGen $ \ds (oId, (vx, vy)) -> do
	let dt = realToFrac (dtime ds)
	deltaMoveObject oId (dt*vx, dt*vy)
	return (Right (), moveObjectR)

----wcSpawnDoor :: WorldContext ()
----wcSpawnPlayer = newObject >>= deltaAddDoor

----wcSpawnPlayer :: WorldContext ()
----wcSpawnPlayer = newObject >>= deltaAddPlayer "Marco" (50, 50)
spawnPlayerAt :: String -> (Float, Float) -> WorldWire a (Event PlayerId)
spawnPlayerAt name pos = mkObjGenWire $ deltaAddPlayer name pos

spawnWallAt pos = mkObjGenWire $ deltaAddWall pos

--spawnDoor :: WorldWire a (Event DoorId)
--spawnDoor = mkObjGenWire $ deltaAddDoor (20, 20)

nullObject :: ObjectId
nullObject = 0

genLater :: WorldWire (Event ObjectId) (Event ObjectId)
--genLater = delay (Event nullObject)
genLater = delay NoEvent

--spawnDoorController :: DoorId -> WorldWire a (Event DoorControllerId)
--spawnDoorController dId = mkObjGenWire $ deltaAddDoorController dId

--data ControllerState = Started | Finished

--runController :: WorldWire DoorControllerId (Event ControllerState)
--runController = mkGen $ \ds dcId -> do
--	dc <- doorController dcId 
--	let time' = dcTimeRunning dc + realToFrac (dtime ds)
--	if time' > dcTimeNeedsToOpen dc
--		then do
--			deltaUpdateDoorController $ dc 
--				{ dcTimeRunning = 0
--				, dcStarted = False
--				}
--			return (Right $ Event Finished, never)
--		else do
--			deltaUpdateDoorController $ dc 
--				{ dcTimeRunning = time'
--				, dcStarted = True
--				}
--			return (Right NoEvent, runController)

--openDoor = until . fmap (\e -> ((), e)) runController

mkObjGenWire :: (ObjectId -> WorldContext ()) -> WorldWire a (Event ObjectId)
mkObjGenWire f = genLater . mkGenN (\_ -> do
		oId <- newObject
		f oId
		return (Right (Event oId), never)
	)

userSpeed = 100

testwire = proc input -> do
	playerId <- asSoonAs . spawnPlayerAt "Marco" (50, 50) -< input
	_ <- asSoonAs . spawnWallAt (60, 60) -< input
	(x, y) <- movingDirectionR -< playerId
	mul <- pure (-1) . for 3 . asSoonAs . objectCollided <|> pure 1 -< playerId
	_ <- moveObjectR -< (playerId, (x*userSpeed*mul, y*userSpeed))
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
		let myManager = manager {
			_wmPlayerActions = Map.insert (1::PlayerId) (newInputAction (ActionMove 0.1 0)) Map.empty
		}
		(quit, (w', session'), (manager', delta)) <- worldLoop w session world myManager
		let world' = applyDelta world delta
		print (world'^.wPositions, quit)
		--print (delta)
		threadDelay 10000
		let quit = False
		Control.Monad.unless quit $ loop world' manager' w' session'
