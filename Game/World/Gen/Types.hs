{-# LANGUAGE NamedFieldPuns, TemplateHaskell, MultiParamTypeClasses #-}
module Game.World.Gen.Types where

import qualified Data.Map as Map
import System.Random
import Data.Maybe
import Debug.Trace
import Test.QuickCheck

import Control.Monad.RWS
import Control.Monad.State
import Control.Lens

-- | Content data typesesp
data Direction =
      North
    | East
    | South
    | West 
    deriving (Show)

data RoomType =
      DefaultRoom
    | BossRoom 
    deriving (Show)

data Door = Door
    { _doorPosition :: (Int, Int)  -- | relative to room
    } deriving (Show)

type StageIndex = Int
data Map = Map
    { _mapStages :: Map.Map StageIndex Stage
    } deriving (Show)

newMap = Map
    { _mapStages = Map.empty
    }

data Difficulty =
      DifficultyTutorial
    | DifficultyEasy
    | DifficultyNormal
    | DifficultyHard
    deriving (Show)

type RoomIndex = Int
data Stage = Stage
    { _stageRooms :: Map.Map RoomIndex Room
    , _stageDifficulty :: Difficulty
    } deriving (Show)

newStage = Stage
    { _stageRooms = Map.empty
    , _stageDifficulty = DifficultyTutorial
    }

type DoorIndex = Int
data Room = Room
    { _roomType :: RoomType
    , _roomSize :: RoomSize
    , _roomDoors :: Map.Map DoorIndex Door
    } deriving (Show)

newRoom = Room 
    { _roomType = DefaultRoom
    , _roomSize = RoomSize 0 0
    , _roomDoors = Map.empty
    }

data RoomSize = RoomSize 
    { _rsWidth :: Int
    , _rsHeight :: Int
    } deriving (Show)

data Corridor = Corridor
    { _corPoints :: [(Int, Int)]
    } deriving (Show)

data Intersection = 
      CorridorIntersection
        { _intersectionCorridors :: Map.Map Direction [Corridor]
        }
    | RoomIntersection
        { _intersectionCorridor :: (Direction, Corridor)
        , _intersectionRoom :: Room
        } 
    deriving (Show)

data Loot =
      NoLoot  
    deriving (Show)

makeLenses ''Room
makeLenses ''Door
makeLenses ''RoomSize
makeLenses ''Corridor
makeLenses ''Intersection
makeLenses ''Map
makeLenses ''Stage

instance Monoid Map where
    mempty = newMap
    mappend m1 m2 = m2

instance Monoid Stage where
    mempty = newStage
    mappend s1 s2 = s2

instance Monoid Room where
    mempty = newRoom
    mappend r1 r2 = r2

-- Generation data types
data GenState = GenState
    { _stateRndGen :: StdGen
    }
makeLenses ''GenState
type GenContext = State GenState

--runGenContext :: 
runGenContext m g = evalRWS (
        m
    ) () $ GenState g

type GenMap = RWS () Map GenState
type GenStage = RWS Map Stage GenState
type GenRoom = RWS (Map, Stage) Room GenState

genRoom :: GenRoom ()
genRoom = do
    return ()

genStage :: GenStage ()
genStage = do
    genState <- get
    map <- ask
    let stage = newStage
    let (_, state, room) = runRWS (genRoom) (map, stage) genState
    put state
    scribe (stageRooms . at 1) (Just room)

genMap :: GenMap ()
genMap = do
    genState <- get
    let map = newMap
    let (_, state, stage) = runRWS (genStage) map genState
    return ()


