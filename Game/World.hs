{-# LANGUAGE FlexibleContexts, Arrows, NamedFieldPuns, Rank2Types #-}
module Game.World 
	(
	-- * World
	  WorldWire, WorldSession, WorldContext
	, World, WorldManager, WorldDelta

	-- * To remove
	, applyDelta

	, newWorldFromTiled
	, testwire
	, updateTiled
	, newRenderObjects
	, update
	, Renderable
	) where

import Game.World.Lens
import Control.Monad.RWS
import Control.Monad.State
import Data.Maybe
import Game.Input.Input

import Control.Wire
import qualified Control.Wire as W
import Control.Wire.Unsafe.Event
import qualified Prelude as P
import Prelude hiding ((.), until)

import Control.Lens
import qualified Control.Lens as L
import Game.World.Objects

import qualified Data.Tiled as T
import Data.Tiled
import Game.World.Import.Tiled


import Game.World.Delta
import Game.World.Wires
import Game.World.Common
import qualified Game.World.Objects as World

whenMaybeDo :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenMaybeDo m f = 
	case m of
		Just v -> f v
		Nothing -> return ()

type Renderer = RWST (World, WorldDelta, [Renderable]) [Renderable] TiledMap IO

newRenderObjects :: Renderer ()
newRenderObjects = do
	(world, delta, _) <- ask

	let newObjects' = delta^.newObjects
	let objectGids = [world^?getAnimations. L.at (o^.objId)._Just.animTileGid | o <- newObjects']
	let objectPoss = [world^?getPositions. L.at (o^.objId)._Just | o <- newObjects']

	mapM_ (\(obj, objGid, pos) -> do
			layerObj.layerObjects <>= buildObject objGid obj pos
			writer ((), [obj])
		) $ zip3 newObjects' objectGids objectPoss

	where
		buildObject objGid obj mpos = ifPosObj
			where
				ifPosObj = case mpos of
					Just _ -> case objGid of 
						Just gid -> object gid
						Nothing -> object (1 :: Int)
					Nothing -> []
				object objectGid' = [T.Object { _objectName= Just $ obj^.objName
							   , _objectGid= Just (fromIntegral objectGid')
							   , _objectX = round $ fst . fromJust $ mpos
							   , _objectY = round $ snd . fromJust $ mpos
							   , _objectWidth = Nothing
							   , _objectHeight = Nothing
							   , _objectType = Nothing
							   , _objectProperties = []
							   , _objectPolygon = Nothing
							   , _objectPolyline = Nothing
							   }]

update :: Renderer ()
update = do
	(world, _, renderables) <- ask
	mapM_ (\obj -> do
			let oId = obj^.objId 
			--let Just oId = fmap _objId (world^.findObject (obj^.))
			let Just oPos = world^.objectPosition oId
			let oGid = world^?getAnimations. L.at oId._Just.animTileGid

			tiledMap <- get
			tiledObject (obj^.objName).objectPos tiledMap .= oPos
			whenMaybeDo oGid (\gid -> 
				tiledObject (obj^.objName).objectGid .= Just (fromIntegral gid))
		) renderables

updateTiled :: Renderer ()
updateTiled = do
	(world, delta, _) <- ask
	tiled <- get
	put (newTiled world delta tiled)
	where
		newTiled world _ tiled = execState (do
				tiledObject "Player1".objectPos tiled .= playerPos
				tiledObject "Player2".objectPos tiled .= player2Pos
				tiledObject "Dino".objectPos tiled .= dinoPos
				tiledObject "Bee".objectPos tiled .= beePos
				whenMaybeDo playerGid (\gid -> 
					tiledObject "Player1".objectGid .= Just (fromIntegral gid))

				whenMaybeDo player2Gid (\gid -> 
					tiledObject "Player2".objectGid .= Just (fromIntegral gid))

				whenMaybeDo dinoGid (\gid -> 
					tiledObject "Dino".objectGid .= Just (fromIntegral gid))

				whenMaybeDo beeGid (\gid -> 
					tiledObject "Bee".objectGid .= Just (fromIntegral gid))
			) tiled
			where
				Just dinoId = fmap _objId (world^.findObject "Dino")
				Just dinoPos = world^.objectPosition dinoId
				dinoGid = world^?getAnimations. L.at dinoId._Just.animTileGid

				Just beeId = fmap _objId (world^.findObject "Bee")
				Just beePos = world^.objectPosition beeId
				beeGid = world^?getAnimations. L.at beeId._Just.animTileGid

				Just pId = fmap _objId (world^.findObject "Neira")
				Just playerPos = world^.objectPosition pId
				playerGid = world^?getAnimations. L.at pId._Just.animTileGid
				--let boulderPos = world'^.wBoulderPos "Boulder1"

				Just p2Id = fmap _objId (world^.findObject "TheGhost")
				Just player2Pos = world^.objectPosition p2Id
				player2Gid = world^?getAnimations. L.at p2Id._Just.animTileGid

layerObj :: Traversal' TiledMap Layer
layerObj = mapLayers.traverse._ObjectLayer
--layerObj1 :: Traversal' RenderContext [Layer]
tiledObject :: String
	-> Traversal' TiledMap Data.Tiled.Object
tiledObject name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.objectsByName name

type Renderable = World.Object

newWorldFromTiled :: TiledMap -> IO (World, WorldManager) -- io due to debug wire
newWorldFromTiled tiledMap = do
	let world = emptyW 
		{ _wTileBoundary = tiledMap^.mapTileSize
		}
	(worldManager, worldDelta) <- execRWST (
			stepWire initWire (Timed 0 ()) (Right ())
		) world emptyWM

	let world' = applyDelta world worldDelta
	--print world'
	return (world', worldManager)

	where
		Just player1Obj = queryObject tiledMap "Player1"
		Just player2Obj = queryObject tiledMap "Player2"
		Just dinoObj = queryObject tiledMap "Dino"
		Just beeObj = queryObject tiledMap "Bee"

		wallPositions = mapWallPositions tiledMap
		--boulders = mapBoulders tiledMap

		--genWalls :: [(Float, Float)] -> WorldWire a b
		genWalls [] = returnA
		genWalls (wallPos:walls) = proc input -> do
			wId <- spawnObjectAt "Wall" wallPos -< input
			genWalls walls -< input
			wLiftSetOnceVoid setStaticCollidable -< wId
			returnA -< input

		--genBoulders [] = returnA
		--genBoulders (boulder:boulders) = proc input -> do
		--	spawnObjectAt "Boulder" (boulder^.objectPos tiledMap) -< input
		--	genBoulders boulders -< input
		--	returnA -< input

		initWire = proc input -> do
			p1Id <- spawnObjectAt "Neira" (player1Obj^.objectPos tiledMap) -< input
			p2Id <- spawnObjectAt "TheGhost" (player2Obj^.objectPos tiledMap) -< input
			dId <- spawnObjectAt "Dino" (dinoObj^.objectPos tiledMap) -< input
			bId <- spawnObjectAt "Bee" (beeObj^.objectPos tiledMap) -< input
			_ <- wLiftSetOnce setBoundary playerBoundary -< p1Id
			_ <- wLiftSetOnce setBoundary playerBoundary -< p2Id
			_ <- wLiftSetOnce setBoundary playerBoundary -< dId
			_ <- wLiftSetOnce setBoundary playerBoundary -< bId

			-- Initialize: need maybe check in client to remove this TODO
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 1
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 2
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 3
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 4

			_ <- genWalls wallPositions -< input
			--_ <- genBoulders boulders -< input

			returnA -< ()

moveArrow :: (Float, Float) -> ObjectWire ObjectId ()
moveArrow direction = proc oId -> do
	_ <- move direction . for 1 -< oId
	returnA -< ()

spawnArrow :: ObjectWire PlayerId ()
spawnArrow = spawn . thenDo (inhibit WireFinished)
	where
		spawn = proc playerId -> do	
			(oId, playerPos, playerDir) <- step1 -< playerId
			_ <- setup -< (oId, playerPos, playerDir, playerId)
			_ <- spawnWire -< (oId, playerDir)

			returnA -< ()

		step1 = proc playerId -> do
			Event oId <- spawnObjectMakeName -< playerId
			Just playerPos <- wLiftF (\pId -> view $ getPositions . L.at pId) -< playerId
			Just playerDir <- wLiftF (\pId -> view $ getOrientations . L.at pId) -< playerId
			returnA -< (oId, playerPos, playerDir)

		spawnWire = proc (oId, playerDir) -> do
			let (dx, dy) = deltaFromOrientation playerDir
			let wire = moveArrow (dx*400, dy*400)
			_ <- newObjectWireR -< (oId, wire)
			returnA -< ()

		setup = proc (oId, playerPos, playerDir, playerId) -> do
			_ <- setPosOnceR -< (oId, playerPos)
			_ <- animateR -< (oId, arrowAnimation playerDir)
			_ <- wLiftSetOnceR setBoundary -< (oId, arrowBoundary playerDir)
			_ <- wLiftSetOnceR setIgnoreCollision -< (oId, playerId)
			_ <- wLiftSetOnceR setIgnoreCollision -< (playerId, oId)
			returnA -< ()

playerSpawnArrow :: ObjectWire PlayerId ()
playerSpawnArrow = untilV spawnArrowEvent
	W.--> spawnArrow 
	W.--> void while . spawnArrowEvent
	W.--> playerSpawnArrow

playerMovement :: ObjectWire PlayerId ()
playerMovement = untilV movingDirectionE 
	W.--> movePlayer
	W.--> playerMovement
	where
		movePlayer = proc pId -> do
			-- Direction of player
			(dx, dy) <- movingDirectionR -< pId

			-- move the player
			_ <- moveR -< (pId, (-dx*200, dy*200))

			-- set new orientation
			let orientation = orientationFromDelta (-dx, dy)
			let anim = objectAnimation pId orientation
			_ <- wLiftUpdateR setOrientation -< (pId, orientation)

			-- animate the player
			_ <- animateR -< (pId, anim)
			returnA -< ()

playerWire :: ObjectWire ObjectId ()
playerWire = proc pId -> do
	_ <- playerSpawnArrow -< pId
	_ <- playerMovement -< pId
	returnA -< ()

dinoWire :: ObjectWire ObjectId ()
dinoWire = proc pId -> do
	_ <- animate (objectAnimation 3 North) -< pId
	returnA -< ()

beeWire :: ObjectWire ObjectId ()
beeWire = proc pId -> do
	_ <- animate (objectAnimation 4 North) -< pId
	returnA -< ()

testwire :: WorldWire a ()
testwire = proc input -> do
	_ <- stepObjectWires -< input
	_ <- once . newObjectWire 1 playerWire -< input
	_ <- once . newObjectWire 2 playerWire -< input
	_ <- once . newObjectWire 3 dinoWire -< input
	_ <- once . newObjectWire 4 beeWire -< input

	returnA -< ()


--worldLoop w'a session' world' state' = do
--	(dt, session) <- stepSession session'
--	((out, w), worldManager, worldDelta) <- runRWST (
--		stepWire w' dt (Right ())
--		) world' state'

--	let quit = case out of
--		Right _ -> False
--		Left _ -> True
--	return (quit, (w, session), (worldManager, worldDelta))
