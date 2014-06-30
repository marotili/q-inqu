module Game.Render.Update where

import Debug.Trace
import Game.World.Lens
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Data.Maybe
import Game.Input.Input

import Control.Wire
import Control.Monad
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
import Game.World.Lens
import Game.World.Common
import qualified Game.World.Objects as World

import Game.Render.World hiding (World)
import qualified Game.Render.World as R

type Renderer = RWS (World, WorldDelta, [Renderable]) [Renderable] R.RenderWorld

-- * TODO: fix monad
whenMaybeDo :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenMaybeDo m f =
	case m of
		Just v -> f v
		Nothing -> return ()

removeRenderObjects = return () :: Renderer ()
newRenderObjects = return () :: Renderer ()
update = return () :: Renderer ()
--removeRenderObjects :: Renderer ()
--removeRenderObjects = do
--	(world, delta, _) <- ask
--	let deleted = delta^.deletedObjects
--	mapM_ (\objId -> do
--			let Just obj = world^.wObjects.L.at objId
--			-- FIXME
--			-- order of deletion is important
--			wLayerObject "CObjectLayer" (obj^.objName) .= Nothing
--			wObject (obj^.objName) .= Nothing
--			mapHashes.gameObjects.L.at (obj^.objName) .= Nothing
--			writer ((), [obj])
--		) deleted

--newRenderObjects :: Renderer ()
--newRenderObjects = do
--	(world, delta, _) <- ask

--	let newObjects' = delta^.newObjects
--	let objectTileNames = [world^?getAnimations. L.at (o^.objId)._Just.animTileName | o <- newObjects']
--	let objectPoss = [world^?getPositions. L.at (o^.objId)._Just | o <- newObjects']

--	mapM_ (\(obj, tileName, pos) -> do
--			-- only objects with a position are renderable
--			Control.Monad.when (isJust pos && isJust tileName
--				) $ do
--					objId <- wObjectFromPrefab "FWTFrontStand" (obj^.objName)
--					wLayerObject "CObjectLayer" (obj^.objName) .= (Just $
--						newRenderObject objId (0, 0) 0)
--					writer ((), [obj])
--		) $ zip3 newObjects' objectTileNames objectPoss

--data RenderableGameObject = RenderableGameObject
--	{ rgoInit :: Renderer ()
--	, rgoUpdate :: Renderer ()
--	}

--update :: Renderer ()
--update = do
--	(world, _, renderables) <- ask
--	mapM_ (\obj -> do
--			let oId = obj^.objId
--			let Just oPos = world^.objectPosition oId
--			let mRot = world^.objectRotation oId

--			tiledMap <- get


--			-- update position
--			wLayerObject "CObjectLayer" (obj^.objName) . _Just . roPos .= oPos
--			case mRot of
--				Just rot -> do
--					wLayerObject "CObjectLayer" (obj^.objName)
--						. _Just . roRotation .= rot
--				Nothing -> return ()

--			-- animation
--			let mTileName = world^?getAnimations. L.at oId._Just.animTileName

--			-- update tile
--			whenMaybeDo mTileName (\tileName -> do
--				-- tileset of tile

--				--let Just (tilesetName, localTileId) =
--				--	tileMap ^. L.at tileName
--				-- wLayerObject "CObjectLayer" (obj^.objName) . _Just . roTileName .= tileName
--				wSetObjectPrefab (obj^.objName) tileName

--				--case tileMap ^. L.at tileName of
--				--	Just (tilesetName, localTileId) -> do
--				--		--Just tsId <- use $ mapHashes . gameTilesets . L.at tilesetName
--				--		--wObject (obj^.objName)._Just.objTsId .= tsId
--				--		--wObject (obj^.objName)._Just.objLocalId .= localTileId
--				--	Nothing -> return ()
--					--Nothing -> do
--					--	Just (tsId, localTileId) <- use $ mapHashes . gamePrefabs . L.at tileName
--					--	wObject (obj^.objName)._Just.objTsId .= tsId
--					--	wObject (obj^.objName)._Just.objLocalId .= localTileId
--				)
--		) renderables

type Renderable = World.Object
