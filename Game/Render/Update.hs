module Game.Render.Update where

import Debug.Trace
import Game.World.Lens
import Control.Monad.RWS
import Control.Monad.State
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
import Game.World.Common
import qualified Game.World.Objects as World

import Game.Render.World hiding (World)
import qualified Game.Render.World as R

type Renderer = RWS (World, WorldDelta, [Renderable]) [Renderable] R.World

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
	let objectTileNames = [world^?getAnimations. L.at (o^.objId)._Just.animTileName | o <- newObjects']
	let objectPoss = [world^?getPositions. L.at (o^.objId)._Just | o <- newObjects']

	mapM_ (\(obj, tileName, pos) -> do
			-- only objects with a position are renderable
			Control.Monad.when (isJust pos && isJust tileName
				) $ do
					wObject (obj^.objName) .= (Just $ R.newObject 4 0)
					Just objId <- use $ mapHashes . gameObjects . L.at (obj^.objName)
					wLayerObject "ObjectLayer" (obj^.objName) .= (Just $
						newRenderObject objId (0, 0) 0)
					writer ((), [obj])
		) $ zip3 newObjects' objectTileNames objectPoss

update :: Renderer ()
update = do
	(world, _, renderables) <- ask
	mapM_ (\obj -> do
			let oId = obj^.objId 
			let Just oPos = world^.objectPosition oId

			tiledMap <- get

			-- update position
			wLayerObject "ObjectLayer" (obj^.objName) . _Just . roPos .= oPos

			-- animation
			let mTileName = world^?getAnimations. L.at oId._Just.animTileName

			-- update tile
			whenMaybeDo mTileName (\tileName -> do
				-- tileset of tile
				tileMap <- use $ 
					R.wRenderConfig . rcTiles
				let Just (tilesetName, localTileId) = 
					tileMap ^. L.at (traceShow (tileMap, tileName) tileName)

				Just tsId <- use $ mapHashes . gameTilesets . L.at tilesetName

				wObject (obj^.objName)._Just.objTsId .= tsId
				wObject (obj^.objName)._Just.objLocalId .= localTileId
				)
		) renderables

type Renderable = World.Object