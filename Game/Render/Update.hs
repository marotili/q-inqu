module Game.Render.Update where

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

import Game.Render.World hiding (World)
import qualified Game.Render.World as R

type Renderer = RWST (World, WorldDelta, [Renderable]) [Renderable] R.World IO

-- * TODO: fix monad
whenMaybeDo :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenMaybeDo m f = 
	case m of
		Just v -> f v
		Nothing -> return ()

removeRenderObjects :: Renderer ()
removeRenderObjects = do
	(world, delta, _) <- ask
	let deleted = delta^.deletedObjects
	lift $ print ("Deleted", deleted)
	mapM_ (\objId -> do
			let Just obj = world^.wObjects.L.at objId
			-- FIXME
			-- order of deletion is important
			wLayerObject "ObjectLayer" (obj^.objName) .= Nothing
			wObject (obj^.objName) .= Nothing
			mapHashes.gameObjects.L.at (obj^.objName) .= Nothing
			writer ((), [obj])
		) deleted

newRenderObjects :: Renderer ()
newRenderObjects = do
	(world, delta, _) <- ask

	let newObjects' = delta^.newObjects
	lift $ print ("New", newObjects')
	let objectGids = [world^?getAnimations. L.at (o^.objId)._Just.animTileGid | o <- newObjects']
	let objectPoss = [world^?getPositions. L.at (o^.objId)._Just | o <- newObjects']

	mapM_ (\(obj, objGid, pos) -> do
			wObject (obj^.objName) .= (Just $ R.newObject 4 0)
			Just objId <- use $ mapHashes . gameObjects . L.at (obj^.objName)
			wLayerObject "ObjectLayer" (obj^.objName) .= (Just $
				newRenderObject objId (0, 0))
			writer ((), [obj])
		) $ zip3 newObjects' objectGids objectPoss

update :: Renderer ()
update = do
	(world, _, renderables) <- ask
	mapM_ (\obj -> do
			let oId = obj^.objId 
			--let Just oId = fmap _objId (world^.findObject (obj^.))
			let Just oPos = world^.objectPosition oId
			let oGid = world^?getAnimations. L.at oId._Just.animTileGid

			tiledMap <- get
			wLayerObject "ObjectLayer" (obj^.objName) . _Just . roPos .= oPos
			whenMaybeDo oGid (\gid -> do
				wObject (obj^.objName)._Just.objTsId .= 4
				wObject (obj^.objName)._Just.objLocalId .= 0
				)
		) renderables

updateTiled :: Renderer ()
updateTiled = do
	(world, delta, _) <- ask
	tiled <- get
	put (newTiled world delta tiled)
	where
		newTiled world _ tiled = execState (do
			wLayerObject "ObjectLayer" "Player1" . _Just . roPos .= playerPos
			--wLayerObject "ObjectLayer" "Player2" . _Just . roPos .= player2Pos
			--wLayerObject "ObjectLayer" "Dino" . _Just . roPos .= playerPos
			--wLayerObject "ObjectLayer" "Bee" . _Just . roPos .= playerPos

			--	whenMaybeDo playerGid (\gid -> 
			--		tiledObject "Player1".objectGid .= Just (fromIntegral gid))

			--	whenMaybeDo player2Gid (\gid -> 
			--		tiledObject "Player2".objectGid .= Just (fromIntegral gid))

			--	whenMaybeDo dinoGid (\gid -> 
			--		tiledObject "Dino".objectGid .= Just (fromIntegral gid))

			--	whenMaybeDo beeGid (\gid -> 
			--		tiledObject "Bee".objectGid .= Just (fromIntegral gid))
			--) tiled
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

type Renderable = World.Object