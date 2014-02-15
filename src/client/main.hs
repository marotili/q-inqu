module Main (main) where

--------------------------------------------------------------------------------

import GHC.Float
import System.Environment

import Control.Concurrent.STM    (TQueue, TVar, newTVarIO, readTVar, atomically, newTQueueIO, tryReadTQueue, writeTQueue, readTQueue)
import Control.Monad             (unless, when, void)
import Control.Monad.Reader
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List                 (intercalate)
import Data.Maybe                (catMaybes)
import qualified Control.Wire as W
--import Control.Monoid
import qualified FRP.Netwire as NW
import qualified Data.Set as Set

import qualified Graphics.UI.GLFW          as GLFW
import Game.World.Import.Tiled
import Game.Input.Input
import Game.Input.Actions
import Text.PrettyPrint
--import Game.Render
--import Game.Cell
import qualified Game.Render as Render
import Game.Render.Map
import qualified Game.Render.Map as RMap
import Game.Render.Camera
--import Game.Unit
--import Game.Graph (step, genTestGraph, Graph, test)
import qualified Graphics.Rendering.OpenGL as GL
import Linear

import qualified Control.Monad.State as S

import Game.Network.Client

import Network.Simple.TCP
import Control.Concurrent

import Data.Tiled
 
import Pipes as P
import Pipes.Network.TCP
import Pipes.Concurrent
import Control.Concurrent.Async
import Pipes.Binary
import Game.World
import qualified Pipes.Binary as PB
import Game.Render.Error
import qualified Data.ByteString as B
import qualified Control.Monad.Trans.State.Strict as StrictState
--------------------------------------------------------------------------------

data Env = Env
    { envEventsChan    :: TQueue Event
    , envActionChan    :: TQueue (Float, InputActions)
    , envWindow        :: !GLFW.Window
    , envRenderContext :: TVar Render.RenderContext
    }

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    
    , stateCam :: Camera
    , stateGameMap :: RMap.Map
    , stateInput :: S.State UserInput ()
    }

type Demo = RWST Env () State IO

--------------------------------------------------------------------------------

data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

--actionProducer :: Producer InputActions 
actionProducer ac playerId = do
    lift $ print "Read action chan"
    mtimeactions <- liftIO $ atomically $ tryReadTQueue ac
    case mtimeactions of
      Just timeactions -> do
        --case mtimeactions of
        let (time, InputActions actions) = timeactions
        lift $ print "Send actions"
        mapM_ (\a -> P.yield (time, playerId, a)) (Set.toList actions)
          --Nothing -> return ()
            --lift . threadDelay $ 100000
      Nothing -> lift $ threadDelay 10000

    actionProducer ac playerId

decodeId :: StrictState.StateT (Producer B.ByteString IO ()) IO (Either PB.DecodingError (PB.ByteOffset, Int))
decodeId = PB.decode

main :: IO ()
main = withSocketsDo $ do
    let width  = 640
        height = 480

    _ <- writeFile "timelog" ""

    args <- getArgs
    let ip = case args of [] -> "127.0.0.1"; _ -> head args

    connect ip "5002" $ \(sock, addr) -> do
      let fromServer = fromSocket sock 4096
      let toServer = toSocket sock
      actionsChan <- newTQueueIO :: IO (TQueue (Float, InputActions))

      let fromServerFirst = fromSocket sock 4096 -- :: Producer ByteString IO ()
      print "Send player id"
      (mres, _) <- StrictState.runStateT decodeId fromServerFirst
      let playerId = case mres of Right (_, b) -> b+1; _ -> -1 -- playerIds are 1 and 2, we receive 0 or
      print ("Sent player id", playerId)

      eventsChan <- newTQueueIO :: IO (TQueue Event)

      withWindow width height "GLFW-b-demo" $ \win -> do
          GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
          GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
          GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
          GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
          GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
          GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
          GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
          GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
          GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
          GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
          GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
          GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
          GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
          GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

          GLFW.swapInterval 1

          initLogging
          printInformation win

          (fbWidth, fbHeight) <- GLFW.getFramebufferSize win

          -- generate map from tiled
          tiledMap <- loadMapFile "data/sewers.tmx"
          --let m = mapNew $ mapConfigFromTiled tiledMap
          let rm = newRenderMap tiledMap

          rc <- Render.newRenderContext rm
          -- default render context
          renderContext <- newTVarIO rc

          async $ do
            runEffect $ for (actionProducer actionsChan playerId) PB.encode >-> toServer
            performGC

          --let f e = do
          --      case e of
          --        Left de -> lift $ print $ fst de
          --        _ -> return ()

          --let repeatDecode = for (decodeSteps fromServer >>= f) (\x -> lift (print $ "In: " ++ show x))

          forkIO $ do
            (world, manager) <- newWorldFromTiled tiledMap
            runEffect $ decodeSteps fromServer >-> consumeClientWorld world manager testwire renderContext []
            performGC

          let 
              env = Env
                { envEventsChan    = eventsChan
                , envActionChan = actionsChan
                , envWindow        = win
                , envRenderContext = renderContext
                }
              state = State
                { stateWindowWidth     = fbWidth
                , stateWindowHeight    = fbHeight
                , stateInput = return ()
                , stateCam = newDefaultCamera (fromIntegral fbWidth) (fromIntegral fbHeight)
                , stateGameMap = rm
                }
          runDemo env state

      --mapM_ wait [a1]

      putStrLn "ended!"

--------------------------------------------------------------------------------

-- GLFW-b is made to be very close to the C API, so creating a window is pretty
-- clunky by Haskell standards. A higher-level API would have some function
-- like withWindow.

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    --GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    --GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    --GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3

    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

--------------------------------------------------------------------------------

runDemo :: Env -> State -> IO ()
runDemo env state =
    void $ evalRWST (adjustWindow >> run 0 W.clockSession_ userInput) env state 

run :: (Num a, Show a, Show b) 
  => Int 
  -> W.Session IO (W.Timed W.NominalDiffTime ()) 
  -> InputWire a b 
  -> Demo ()
run i session w = do
    lift $ print "Run begin"
    win <- asks envWindow

    Just start <- liftIO GLFW.getTime


    lift $ print "Render begin"
    -- render
    draw

    lift $ print "Process events"
    liftIO $ do
        GLFW.swapBuffers win
        --GL.flush  -- not necessary, but someone recommended it
        GLFW.pollEvents
    processEvents

    state <- get

    userTime2 <- liftIO GLFW.getTime

    let userTime = case userTime2 of Just time -> realToFrac time; Nothing -> 0

    -- user input
    lift $ print "Start handle input"
    let input = asks stateInput state -- maybe not threadsafe
    (actions@(InputActions as), session', w') <- liftIO $ stepInput w session input
    ac <- asks envActionChan
    lift $ print "Wait action chan"
    unless (null (Set.toList as)) $ liftIO . atomically . writeTQueue ac $ (userTime, actions)
    lift $ print "End handle input"

    -- update camera
    let c = asks stateCam state
    let V2 cx cy = screenToOpenGLCoords c 0 0

    lift $ print "Finalizing"
    q <- liftIO $ GLFW.windowShouldClose win
    --liftIO performGC
    --when (i `mod` 20 == 0) $ liftIO performGC

    -- time
    Just end <- liftIO GLFW.getTime
    _ <- liftIO $ appendFile "timelog" ("Time: " ++ show i ++ " / " ++ show (end - start) ++ "\n")

    -- delay
    liftIO $ threadDelay $ 1000 * (1000 `div` 60 - 1000 * round (end - start))
    unless q (run (i+1) session' w')

processEvents :: Demo ()
processEvents = do
    tc <- asks envEventsChan
    me <- liftIO $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do 
        processEvent e
        processEvents
      Nothing -> return ()

processEvent :: Event -> Demo ()
processEvent ev =
    case ev of
      (EventError e s) -> do
        liftIO $ print $ "Error :" ++ show (e, s)
        win <- asks envWindow
        liftIO $ GLFW.setWindowShouldClose win True

      (EventWindowSize _ width height) ->
          modify $ \s -> s { stateCam = cameraUpdateProjection (fromIntegral width) (fromIntegral height) (stateCam s) }
      (EventFramebufferSize _ width height) -> do
          modify $ \s -> s
            { stateWindowWidth  = width
            , stateWindowHeight = height
            }
          adjustWindow

      (EventMouseButton _ mb mbs mk) ->
          modify (if mbs == GLFW.MouseButtonState'Pressed
              then \s -> s { stateInput = stateInput s >> inputMouseButtonDown mb }
              else \s -> s { stateInput = stateInput s >> inputMouseButtonUp mb }
            )

      (EventCursorPos _ x y) -> do
          let x' = round x :: Int
              y' = round y :: Int
          state <- get

          let c = asks stateCam state
          let V2 cx cy = screenToOpenGLCoords c (double2Float x) (double2Float y)
          let V4 wx wy _ _ = cameraInverse c (V4 cx cy 0 1 :: V4 Float)

          modify $ \s -> s { stateInput = stateInput s >> inputUpdateMousePos (x, y) }

      (EventKey win k scancode ks mk) -> do
          when (ks == GLFW.KeyState'Pressed) $ do
              -- Q, Esc: exit
              when (k == GLFW.Key'Escape) $
                liftIO $ GLFW.setWindowShouldClose win True

              modify $ \s -> s { stateInput = stateInput s >> inputKeyDown k }

          when (ks == GLFW.KeyState'Released) $ 
              modify $ \s -> s { stateInput = stateInput s >> inputKeyUp k }
      _ -> return ()

adjustWindow :: Demo ()
adjustWindow = return ()

--- DRAW
-------------------------
draw :: Demo ()
draw = do
  win <- asks envWindow
  state <- get

  rcVar <- asks envRenderContext
  rc <- lift $ atomically $ readTVar rcVar

  let cam = asks stateCam state

  lift $ print "Client: Render"
  liftIO $ Render.render win rc cam
  lift $ print "Client: Render finished"

  return ()

printInformation :: GLFW.Window -> IO ()
printInformation win = do
    version <- GLFW.getVersion
    versionString <- GLFW.getVersionString
    --monitorInfos <- runMaybeT getMonitorInfos
    --joystickNames <- getJoystickNames
    clientAPI <- GLFW.getWindowClientAPI win
    cv0 <- GLFW.getWindowContextVersionMajor win
    cv1 <- GLFW.getWindowContextVersionMinor win
    cv2 <- GLFW.getWindowContextVersionRevision win
    robustness <- GLFW.getWindowContextRobustness win
    forwardCompat <- GLFW.getWindowOpenGLForwardCompat win
    debug <- GLFW.getWindowOpenGLDebugContext win
    profile <- GLFW.getWindowOpenGLProfile win

    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "GLFW C library:" $+$
        nest 4 (
          text "Version:" <+> renderVersion version $+$
          text "Version string:" <+> renderVersionString versionString
        ) $+$
        --text "Monitors:" $+$
        --nest 4 (
          --renderMonitorInfos monitorInfos
        --) $+$
        --text "Joysticks:" $+$
        --nest 4 (
          --renderJoystickNames joystickNames
        --) $+$
        text "OpenGL context:" $+$
        nest 4 (
          text "Client API:" <+> renderClientAPI clientAPI $+$
          text "Version:" <+> renderContextVersion cv0 cv1 cv2 $+$
          text "Robustness:" <+> renderContextRobustness robustness $+$
          text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
          text "Debug:" <+> renderDebug debug $+$
          text "Profile:" <+> renderProfile profile
        ) $+$
        text "------------------------------------------------------------"
      )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
        text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
        text . show

    --renderMonitorInfos =
    --    maybe (text "(error)") (vcat . map renderMonitorInfo)

    --renderMonitorInfo (name, (x,y), (w,h), vms) =
    --    text (show name) $+$
    --    nest 4 (
    --      location <+> size $+$
    --      fsep (map renderVideoMode vms)
    --    )
    --  where
    --    location = int x <> text "," <> int y
    --    size = int w <> text "x" <> int h <> text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
        brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz = int rr <> text "Hz"

    --renderJoystickNames pairs =
    --    vcat $ map (\(js, name) -> text (show js) <+> text (show name)) pairs

    renderContextVersion v0 v1 v2 =
        hcat [int v0, text ".", int v1, text ".", int v2]

    renderClientAPI = text . show
    renderContextRobustness = text . show
    renderForwardCompat = text . show
    renderDebug = text . show
    renderProfile = text . show