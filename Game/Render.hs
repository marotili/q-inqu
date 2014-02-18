{-# LANGUAGE TemplateHaskell #-}
module Game.Render 
	(
	  RenderContext
	, newRenderContext
	, clearWindow, render

	-- * To remove
	, rcWorldRenderContext
	) where

import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Game.Render.Map
import Game.Render.Render
import Game.Render.Camera
import Game.Render.Light

import Control.Lens

import Game.Render.Error
import Game.World.Import.Tiled
import Game.World.Common
import Game.World
import Data.Tiled
import Game.Render.World

data RenderContext = RenderContext
	{ _rcMainProgram :: Program
	, _rcWorldRenderContext :: WorldRenderContext
	, _rcLightContext :: LightContext
	, _rcVisibilityContext :: VisibilityContext
	}
makeLenses ''RenderContext

newRenderContext :: IO RenderContext
newRenderContext = do
	GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
	GL.blend $= GL.Enabled
	logGL "newRenderContext: blend setup failed"

	tm <- tMap
	let renderWorld = loadMapFromTiled tm
	let nWorld = wUpdate (do
			Just tsId <- use $ mapHashes.gameTilesets.at "sprite_klein3"
			wObject "Player1" .= (Just $ newObject tsId 0)
			Just objId <- use $ mapHashes.gameObjects.at "Player1"
			wLayer "ObjectLayer" .= (Just $ newLayer ObjectLayerType)
			wLayerObject "ObjectLayer" "Player1" 
				.= (Just $ newRenderObject objId (50, 50))
		) renderWorld

	program <- setupShaders "shader.vert" "shader.frag"
	_ <- uniformInfo program
	wrc <- newWorldRenderContext nWorld
	lc <- newLightContext

	(world, manager) <- newWorldFromTiled tm

	vc <- newVisibilityContext (world^.wCollisionManager) (0, 0)

	return RenderContext
		{ _rcMainProgram = program
		, _rcWorldRenderContext = wrc
		, _rcLightContext = lc
		, _rcVisibilityContext = vc
		}

clearWindow :: GLFW.Window -> IO ()
clearWindow window = do

	GL.clearColor $= GL.Color4 1 1 1 1
	logGL "clearWindow: clearColor"
	GL.clear [GL.ColorBuffer]
	logGL "clearWindow: clear"
	(width, height) <- GLFW.getFramebufferSize window
	GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
	logGL "clearWindow: viewport"

render :: GLFW.Window -> RenderContext -> Camera -> IO ()
render window rc cam = do
	-- update camera in every frame for now
	--(width, height) <- GLFW.getFramebufferSize window
	clearWindow window

	logGL "render: set current program"

	let world = rc^.rcWorldRenderContext.wrcWorld
	let Just (x, y) = world^?wLayerObject "ObjectLayer" "Player1"._Just.roPos
	let newCam = cameraUpdatePosition cam (-x) (-y)

	let newRc = rc -- & rcLightContext.lcLights._head.lightPosition .~ (-x, y)

	--GL.stencilTest $= GL.Enabled
	--GL.stencilFunc $= (GL.Never, 1, 255)
	--GL.stencilOp $= (GL.OpReplace, GL.OpKeep, GL.OpKeep)
	--GL.clearStencil $= 0
	--GL.stencilMask $= 255
	--GL.clear [GL.StencilBuffer]

	--visCtxt <- updateVisibilityContext (newRc^.rcVisibilityContext) (x, y)
	--renderVisibilityContext visCtxt newCam

	--GL.stencilMask $= 0
	--GL.stencilFunc $= (GL.Equal, 0, 255)
	--GL.stencilFunc $= (GL.Equal, 1, 255)

	GL.currentProgram $= Just (newRc^.rcMainProgram)
	programSetViewProjection (newRc^.rcMainProgram) newCam
	updateWorldRenderContext (newRc^.rcWorldRenderContext)
	renderWorldRenderContext (newRc^.rcMainProgram) (newRc^.rcWorldRenderContext)

	GL.stencilTest $= GL.Disabled

	--updateLightContext (newRc^.rcLightContext)
	--renderLightContext (newRc^.rcLightContext) newCam
	--where
	--	object name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.objectsByName name
