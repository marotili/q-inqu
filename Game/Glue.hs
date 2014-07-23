module Game.Glue
(
) where

import qualified Graphics.UI.GLFW as GLFW 

import Game.Render (clearWindow)

data RenderLoopConfig = RenderLoopConfig
    { rlcWindow :: GLFW.Window
    }

renderLoop :: RenderLoopConfig -> IO ()
renderLoop config = 
    clearWindow window