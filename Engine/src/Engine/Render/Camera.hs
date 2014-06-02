{-# LANGUAGE NamedFieldPuns #-}
module Engine.Render.Camera
	(
	  Camera
	, cameraUpdateProjection
	, cameraUpdatePosition
	, programSetViewProjection
	, newDefaultCamera
	, screenToOpenGLCoords
	, cameraInverse
	, cameraSetOriginTopLeft
	) where

import qualified Graphics.Rendering.OpenGL.Raw as GLRaw
import qualified Graphics.Rendering.OpenGL as GL

import Linear
import Foreign.Ptr
import qualified Data.Vector.Storable as V
import Engine.Render.Error
import Control.Monad

--data Rect = Rect
--	{ rectPos :: V2 Float
--	, rectSize :: V2 Float
--	}

--data Viewport = Viewport
--	{ viewportRect :: Rect
--	}

data Projection = OrthogonalProjection
	{ projectionMatrix :: M44 Float
	, projectionWidth :: Float
	, projectionHeight :: Float
	} deriving (Show)

data Camera = Camera 
	{ cameraProjection :: Projection
	, cameraPosition :: V3 Float 
	, cameraOrientation :: Quaternion Float
	} deriving (Show)

cameraUpdatePosition :: Camera -> Float -> Float -> Camera
cameraUpdatePosition cam x y = cam { cameraPosition = V3 x y (-1) }

cameraSetOriginTopLeft :: Camera -> Camera
cameraSetOriginTopLeft cam = cam { cameraPosition = V3 (-width/2.0) (height/2.0) (-1)}
	where
		width = projectionWidth . cameraProjection $ cam
		height = projectionHeight . cameraProjection $ cam

screenToOpenGLCoords :: Camera -> Float -> Float -> V2 Float
screenToOpenGLCoords Camera { cameraProjection } x y = V2 2 (-2) * V2 ((x/w) - 0.5) ((y/h) - 0.5)
	where
		w = projectionWidth cameraProjection
		h = projectionHeight cameraProjection

--screenToWorldCoords :: Camera -> Float -> Float -> V2 Float
--screenToWorldCoords (cam@Camera { cameraPosition }) x y = v2*w v2*h
--	where
--		V2 vx vy = screenToOpenGLCoords cam x y
--		w = projectionWidth cameraProjection
--		h = projectionHeight cameraProjection

cameraInverse :: Camera -> V4 Float -> V4 Float
cameraInverse cam vec = (invViewM !*! invProjM) !* vec
	where
		invProjM = invProjM'
			where
			(V4
				(V4 m00 _ _ _)
				(V4 _ m11 _ _)
				(V4 _ _ m22 _)
				(V4 {})) = projectionMatrix . cameraProjection $ cam

			invProjM' = V4
				(V4 (1/m00) 0 0 0)
				(V4 0 (1/m11) 0 0)
				(V4 0 0 (1/m22) 0)
				(V4 0 0 0 1)

		invViewM = invViewM'
			where
			(V4
				(V4 _ _ _ m30)
				(V4 _ _ _ m31)
				(V4 _ _ _ m32)
				(V4 {})) = viewMatrix cam
			invViewM' = V4
				(V4 1 0 0 (-m30))
				(V4 0 1 0 (-m31))
				(V4 0 0 1 (-m32))
				(V4 0 0 0 1)

newDefaultCamera :: Float -> Float -> Camera
newDefaultCamera viewportWidth viewportHeight = Camera
	{ cameraProjection = newOrthogonalProjectionMatrix viewportWidth viewportHeight
	, cameraPosition = V3 0 0 (-1)
	, cameraOrientation = axisAngle (V3 0 0 1) 0
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
		f = 100
		n = 0

programSetViewProjection :: GL.Program -> Camera -> IO ()
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
	unless (viewLoc == -1) $ do
		V.unsafeWith workaround2 $ \ptr -> GLRaw.glUniformMatrix4fv viewLoc 1 1 (castPtr ptr)
		logGL "programSetViewProjection: gl raw matrix view"
