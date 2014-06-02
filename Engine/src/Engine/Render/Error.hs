module Engine.Render.Error where

import System.Log.Logger
import Control.Monad
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import qualified Graphics.Rendering.OpenGL as GL

initLogging :: IO ()
initLogging = do
    h <- fileHandler "opengl.log" ERROR >>= \lh -> return $
        setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "OpenGL" (addHandler h >> setLevel DEBUG)
    --updateGlobalLogger "OpenGL" (setLevel DEBUG)

logGL :: String -> IO ()
logGL msg = do
    errors <- GL.get GL.errors    
    unless (null errors) $ 
        errorM "OpenGL" $ msg ++ " with: " ++ show errors
