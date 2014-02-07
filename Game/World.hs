{-# LANGUAGE FlexibleContexts, TemplateHaskell, Arrows, NamedFieldPuns, Rank2Types #-}
module Game.World 
	(
	-- * World
	  WorldWire, WorldSession, WorldContext
	, World, WorldManager, WorldDelta

	-- * To remove
	, wmPlayerActions
	, applyDelta

	, wPlayerId, wPlayerPos

	, newWorld, newWorldManager, newWorldFromTiled
	, testwire
	) where

import Debug.Trace
import Control.Concurrent

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad
import Data.Maybe

import qualified Data.Binary as B

import Control.Wire
import qualified Control.Wire as W
import Control.Wire.Unsafe.Event
import qualified Prelude as P
import Prelude hiding ((.), until)

import Control.Lens
import qualified Control.Lens as L
import Game.World.Objects
import Game.Collision

import Data.Tiled
import Game.World.Import.Tiled

import Game.Input.Actions as A

import Game.World.Delta

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
	--, _wCollisionManager :: CollisionManager

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
	--, _wCollisionManager = cmNew
	, _wCurrentCollisions = Map.empty
	--, wMap = gameMap
	}

wPlayerId :: String -> Getter World (Maybe PlayerId)
wPlayerId playerName' = to (\w -> case getPlayer w of
			[pId] -> Just pId
			_ -> Nothing
		)
	where
		getPlayer world = ifoldMap (\pId Player { playerName } ->
				[pId | playerName == playerName']
			) (world^.wPlayers)

wPlayerPos :: String -> Getter World (Maybe (Float, Float))
wPlayerPos playerName' = to (\w -> case w^.pId of
			Just _ -> w^.wPositions . L.at (fromJust $ w^.pId)
			Nothing -> Nothing
		)
	where
		pId = wPlayerId playerName'


newWorldFromTiled :: TiledMap -> IO (World, WorldManager) -- io due to debug wire
newWorldFromTiled tiledMap = do
	let world = newWorld
	(worldManager, worldDelta) <- execRWST (
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

movingDirectionR :: WorldWire PlayerId (Float, Float)
movingDirectionR = mkGenN $ \playerId -> do
	wm <- get
	if Map.member playerId (wm^.wmPlayerActions) 
		then do
			let playerActions = asks _wmPlayerActions wm Map.! playerId
			let direction = A.movingDirection playerActions
			return (Right direction, movingDirectionR)
		else
			return (Right (0, 0), movingDirectionR)



applyDelta :: World -> WorldDelta -> World
applyDelta w wd = collisions
	where
		newWalls = w { _wWalls = foldr (\d -> Map.insert (wallId d) d) (_wWalls w) (_wdWallsAdd wd) }
		newPlayers = newWalls { _wPlayers = foldr (\p -> Map.insert (playerId p) p) (_wPlayers newWalls) (_wdPlayerAdd wd) }
		positions = newPlayers { _wPositions = foldr (\(k, v) m -> Map.alter (alterPos v) k m) (_wPositions newPlayers) (Map.toList . deltaPos . _wdPositionsDelta $ wd) }
		--collidables = positions { _wCollisionManager = execState (do
				--mapM_ cmAddStatic [newCollidable oId (newBoundary (objectPos oId) 5) | oId <- map wallId (_wdWallsAdd wd)]
				--mapM_ cmAddFloating [newCollidable oId (newBoundary (objectPos oId) 5) | oId <- map playerId (_wdPlayerAdd wd)]
			--) (_wCollisionManager positions) }
		--floatingCollidables = positions { _wCollisionManager = execState (
		--		mapM_ ((\oId -> cmUpdateFloating oId (objectPos oId)) . fst) (Map.toList (wd^.wdPositionsDelta))
		--	) (_wCollisionManager positions) }

		-- overwrite old ones
		collisions = positions { _wCurrentCollisions = containerData $ wd^.wdCollisions }

		objectPos oId = (positions ^. wPositions) Map.! oId
		--doorControllers = positions { wDoorControllers = foldr (\dc -> Map.insert (doorControllerId dc) dc) (wDoorControllers positions) (wdDoorControllers wd) }

--validateWorldDelta :: WorldDelta -> Bool

-- TODO
-- fix collisions (reset positions)
--fixWorldDelta :: World -> WorldDelta -> WorldDelta

alterPos (x, y) Nothing = Just (x, y)
alterPos (x, y) (Just (x', y')) = Just (x + x', y + y')


newObject :: MonadState WorldManager m => m ObjectId
newObject = do
	wm <- get 
	let oId = wm^.wmNextObjectId
	wmNextObjectId += 1
	wmObjects .= Set.insert oId (wm^.wmObjects)
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
	liftIO $ print "Moving object"
	let dt = realToFrac (dtime ds)
	deltaMoveObject oId (dt*vx, dt*vy)
	liftIO $ print "finished moving object"
	return (Right (), moveObjectR)

spawnPlayerAt :: String -> (Float, Float) -> WorldWire a (Event PlayerId)
spawnPlayerAt name pos = mkObjGenWire $ deltaAddPlayer name pos

spawnWallAt pos = mkObjGenWire $ deltaAddWall pos

player name = mkGenN $ \_ -> do
	pid <- magnify (wPlayerId name) ask
	case pid of 
		Just pId -> return (Right pId, player name)
		Nothing -> return (Left (), player name)

nullObject :: ObjectId
nullObject = 0

genLater :: WorldWire (Event ObjectId) (Event ObjectId)
genLater = delay NoEvent


mkObjGenWire :: (ObjectId -> WorldContext ()) -> WorldWire a (Event ObjectId)
mkObjGenWire f = genLater . mkGenN (\_ -> do
		oId <- newObject
		f oId
		return (Right (Event oId), never)
	)

userSpeed = 100

testwire = proc input -> do
	playerId <- player "Neira" -< input
	--_ <- asSoonAs . spawnWallAt (60, 60) -< input
	(x, y) <- movingDirectionR -< playerId
	--mul <- pure (-1) . for 3 . asSoonAs . objectCollided <|> pure 1 -< playerId
	_ <- moveObjectR -< (playerId, (x*userSpeed, y*userSpeed))
	returnA -< ()

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
		let quit = False
		Control.Monad.unless quit $ loop world' manager' w' session'
