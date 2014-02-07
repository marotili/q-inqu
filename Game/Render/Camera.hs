{-# LANGUAGE NamedFieldPuns #-}
module Game.Render.Camera
	(
	  Camera, Viewport
	, cameraUpdateProjection
	, programSetViewProjection
	, newDefaultCamera
	, screenToOpenGLCoords
	, cameraInverse
	) where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import Linear
import Foreign.Ptr
import qualified Data.Vector.Storable as V

import Debug.Trace
import Game.Render.Error

data Rect = Rect
	{ rectPos :: V2 Float
	, rectSize :: V2 Float
	}

data Viewport = Viewport
	{ viewportRect :: Rect
	}

data Projection = OrthogonalProjection
	{ projectionMatrix :: M44 Float
	, projectionWidth :: Float
	, projectionHeight :: Float
	}

data Camera = Camera 
	{ cameraProjection :: Projection
	, cameraPosition :: V3 Float 
	, cameraOrientation :: Quaternion Float
	}

cameraUpdatePosition cam x y = cam { cameraPosition = V3 x y (-1) }

screenToOpenGLCoords :: Camera -> Float -> Float -> V2 Float
screenToOpenGLCoords Camera { cameraProjection } x y = V2 2 (-2) * V2 ((x/w) - 0.5) ((y/h) - 0.5)
	where
		w = projectionWidth cameraProjection
		h = projectionHeight cameraProjection

cameraInverse :: Camera -> V4 Float -> V4 Float
cameraInverse cam vec = (invViewM !*! invProjM) !* vec
	where
		invProjM = invProjM'
			where
			(V4
				(V4 m00 m10 m20 m30)
				(V4 m01 m11 m21 m31)
				(V4 m02 m12 m22 m32)
				(V4 m03 m13 m23 m33)) = projectionMatrix . cameraProjection $ cam

			invProjM' = V4
				(V4 (1/m00) 0 0 0)
				(V4 0 (1/m11) 0 0)
				(V4 0 0 (1/m22) 0)
				(V4 0 0 0 1)

		invViewM = invViewM'
			where
			(V4
				(V4 m00 m10 m20 m30)
				(V4 m01 m11 m21 m31)
				(V4 m02 m12 m22 m32)
				(V4 m03 m13 m23 m33)) = viewMatrix cam
			invViewM' = V4
				(V4 1 0 0 (-m30))
				(V4 0 1 0 (-m31))
				(V4 0 0 1 (-m32))
				(V4 0 0 0 1)

newDefaultCamera viewportWidth viewportHeight = Camera
	{ cameraProjection = newOrthogonalProjectionMatrix viewportWidth viewportHeight
	, cameraPosition = V3 0 0 (-1)
	, cameraOrientation = axisAngle (V3 0 0 1) (-3.14/2.0)
	}

cameraUpdateProjection :: Float -> Float -> Camera -> Camera
cameraUpdateProjection viewportWidth viewportHeight cam = cam
	{ cameraProjection = newOrthogonalProjectionMatrix viewportWidth viewportHeight 
	}

viewMatrix :: Camera -> M44 Float
viewMatrix Camera { cameraPosition, cameraOrientation } =
	mkTransformation cameraOrientation cameraPosition

newOrthogonalProjectionMatrix :: Float -> Float -> Projection
newOrthogonalProjectionMatrix width height = OrthogonalProjection
	{ projectionMatrix = V4
		(V4 (2/width) 0 0 0)
		(V4 0 (2/height) 0 0)
		(V4 0 0 (-2/(f-n)) (-(f+n)/(f-n)))
		(V4 0 0 0 1)
	, projectionWidth = width
	, projectionHeight = height
	}
	where
		fov = 3.14/3.0
		f = 100
		n = 0.1

programSetViewProjection program camera = do
	let projMat = projectionMatrix . cameraProjection $ camera
	let viewMat = viewMatrix camera	

	let workaround = V.fromList [projMat]
	let workaround2 = V.fromList [viewMat]

	(GL.UniformLocation projLoc) <- GL.get $ GL.uniformLocation program "projection"
	logGL "programSetViewProjection: uniformLocation projection"
	--GL.uniform projLoc $= projMat
	(GL.UniformLocation viewLoc) <- GL.get $ GL.uniformLocation program "view"
	logGL "programSetViewProjection: uniformLocation view"
	--GL.uniform viewLoc $= viewMat

	--print (projLoc, viewLoc)

	V.unsafeWith workaround $ \ptr -> GLRaw.glUniformMatrix4fv projLoc 1 1 (castPtr ptr)
	logGL "programSetViewProjection: gl raw matrix projection"
	V.unsafeWith workaround2 $ \ptr -> GLRaw.glUniformMatrix4fv viewLoc 1 1 (castPtr ptr)
	logGL "programSetViewProjection: gl raw matrix view"
