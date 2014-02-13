{-# LANGUAGE FlexibleContexts, TemplateHaskell, Arrows, NamedFieldPuns, Rank2Types #-}
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

import Debug.Trace
import Control.Concurrent
import Game.World.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Game.Input.Input

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

import qualified Data.Tiled as T
import Data.Tiled
import Game.World.Import.Tiled

import Game.Input.Actions as A

import Game.World.Types

import Game.World.Delta
import Game.World.Wires
import Game.World.Common
import qualified Game.World.Objects as World

import Debug.Trace

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
					Just pos -> case objGid of 
						Just gid -> object gid
						Nothing -> object 1
					Nothing -> []
				object objGid = [T.Object { _objectName= Just $ obj^.objName
							   , _objectGid= Just (fromIntegral objGid)
							   , _objectX = fromIntegral . round $ fst . fromJust $ mpos
							   , _objectY = fromIntegral . round $ snd . fromJust $ mpos
							   , _objectWidth = Nothing
							   , _objectHeight = Nothing
							   }]

update :: Renderer ()
update = do
	(world, delta, renderables) <- ask
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

updateTiled :: World -> WorldDelta -> TiledMap -> TiledMap
updateTiled world delta tiled = newTiled
	where
		newTiled = execState (do
				tiledObject "Player1".objectPos tiled .= playerPos
				tiledObject "Player2".objectPos tiled .= player2Pos
				whenMaybeDo playerGid (\gid -> 
					tiledObject "Player1".objectGid .= Just (fromIntegral gid))

				whenMaybeDo player2Gid (\gid -> 
					tiledObject "Player2".objectGid .= Just (fromIntegral gid))
			) tiled


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

	--print worldDelta

	let world' = applyDelta world worldDelta
	--print world'
	return (world', worldManager)

	where
		Just player1Obj = queryObject tiledMap "Player1"
		Just player2Obj = queryObject tiledMap "Player2"
		Just dinoObj = queryObject tiledMap "Dino"
		Just beeObj = queryObject tiledMap "Bee"

		wallPositions = mapWallPositions tiledMap
		boulders = mapBoulders tiledMap

		--genWalls :: [(Float, Float)] -> WorldWire a b
		genWalls [] = returnA
		genWalls (wallPos:walls) = proc input -> do
			spawnObjectAt "Wall" wallPos -< input
			genWalls walls -< input
			returnA -< input

		genBoulders [] = returnA
		genBoulders (boulder:boulders) = proc input -> do
			spawnObjectAt "Boulder" (boulder^.objectPos tiledMap) -< input
			genBoulders boulders -< input
			returnA -< input

		initWire = proc input -> do
			_ <- spawnObjectAt "Neira" (player1Obj^.objectPos tiledMap) -< input
			_ <- spawnObjectAt "TheGhost" (player2Obj^.objectPos tiledMap) -< input
			_ <- spawnObjectAt "Dino" (dinoObj^.objectPos tiledMap) -< input
			_ <- spawnObjectAt "Bee" (beeObj^.objectPos tiledMap) -< input

			-- Initialize: need maybe check in client to remove this TODO
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 1
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 2
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 3
			--_ <- animate (defaultCharacterAnim (0, 0)) -< 4

			_ <- genWalls wallPositions -< input
			_ <- genBoulders boulders -< input

			returnA -< ()

moveArrow = proc oId -> do
	_ <- move (0, 100) . for 2 -< oId
	returnA -< ()

spawnArrow = spawn . thenDo (inhibit WireFinished)
	where
		spawn = proc input -> do	
			Event oId <- spawnObjectMakeName -< input
			_ <- move (50, 50) -< oId
			_ <- animate arrowAnim -< oId
			_ <- newObjectWireR moveArrow -< oId
			returnA -< ()

playerSpawnArrow = untilV spawnArrowEvent
	W.--> spawnArrow 
	W.--> void while . spawnArrowEvent
	W.--> playerSpawnArrow

playerMovement = untilV movingDirectionE 
	W.--> move
	W.--> playerMovement
	where
		move = proc pId -> do
			(dx, dy) <- movingDirectionR -< pId
			_ <- moveR -< (pId, (-dx*200, dy*200))
			let anim = if pId == 1 
				then defaultCharacterAnim1 (dx, dy)
				else defaultCharacterAnim2 (dx, dy)
			_ <- animateR -< (anim, pId)
			returnA -< ()

playerWire :: ObjectWire ObjectId ()
playerWire = proc pId -> do
	_ <- playerSpawnArrow -< pId
	_ <- playerMovement -< pId
	returnA -< ()

testwire :: WorldWire a ()
testwire = proc input -> do
	_ <- stepObjectWires -< input
	_ <- once . newObjectWire 1 playerWire -< input
	_ <- once . newObjectWire 2 playerWire -< input

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
