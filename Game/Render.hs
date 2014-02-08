{-# LANGUAGE TemplateHaskell #-}
module Game.Render 
	(
	  RenderContext
	, newRenderContext
	, clearWindow, render

	-- * To remove
	, rcWorldRenderContext
	) where

import System.Log.Logger
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import qualified Data.Vector.Storable as V
import Foreign.Storable
import Foreign.Ptr
import Data.Int

import Game.Render.Map
import Game.Render.Render
import Game.Render.Camera
import Codec.Picture.Png
import Codec.Picture

import System.Exit
import Control.Lens

import Game.Render.Error
import Game.World.Import.Tiled
import Data.Tiled

data RenderContext = RenderContext
	{ _rcMainProgram :: Program
	, _rcWorldRenderContext :: WorldRenderContext
	}
makeLenses ''RenderContext

newRenderContext renderMap = do
	GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
	GL.blend $= GL.Enabled
	logGL "newRenderContext: blend setup failed"

	program <- setupShaders

	wrc <- newWorldRenderContext renderMap
	bindWorldRenderContext wrc program

	return RenderContext
		{ _rcMainProgram = program
		, _rcWorldRenderContext = wrc
		}

clearWindow window = do
	GL.clearColor $= GL.Color4 1 1 1 1
	logGL "clearWindow: clearColor"
	GL.clear [GL.ColorBuffer]
	logGL "clearWindow: clear"
	(width, height) <- GLFW.getFramebufferSize window
	GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
	logGL "clearWindow: viewport"

render window rc cam = do
	-- update camera in every frame for now
	(width, height) <- GLFW.getFramebufferSize window
	clearWindow window

	GL.currentProgram $= Just (rc^.rcMainProgram)
	logGL "render: set current program"

	let [playerPos@(x, y)] = rc^..rcWorldRenderContext.wrcMap.tiledMap.object "Player1".objectPos
	let newCam = cameraUpdatePosition cam (-x) (y)
	programSetViewProjection (rc^.rcMainProgram) newCam

	updateWorldRenderContext (rc^.rcWorldRenderContext)
	renderWorldRenderContext (rc^.rcMainProgram) (rc^.rcWorldRenderContext)

	where
		object name = mapLayers.traverse._ObjectLayer.layerObjects.traverse.objectsByName name
