{-# LANGUAGE FlexibleInstances, NamedFieldPuns, TemplateHaskell #-}
module Game.World.Objects where
	--( 
	--  ObjectId, ObjectIds, DoorId, DoorControllerId, SwitchId
	--, PlayerId, WallId, BoulderId
	--, Object(..), objId, objName
	--, Player(..)
	--, Wall(..)
	--, Door(..)
	--, DoorController(..)
	--, Switch(..)
	--, Boulder(..)
	--, ObjectPhysics(..), objectAcceleration, objectSpeed
	--, Animation(..), animTime, animCurrentTime, animNext, animTileGid
	--, defaultCharacterAnim, beeAnim, dinoAnim, arrowAnim
	--, tuple
	--) where

import qualified Data.Set as Set
import Linear
import Debug.Trace
import Control.Lens
import Data.Monoid

type ObjectIds = Set.Set ObjectId
type ObjectId = Int

type DoorId = ObjectId
type DoorControllerId = ObjectId
type SwitchId = ObjectId
type PlayerId = ObjectId
type WallId = ObjectId
type BoulderId = ObjectId

type Position = (Float, Float)

newtype One a = One { unOne :: Maybe a }
instance Monoid (One a) where
    mempty = One Nothing
    mappend (One Nothing) (One x) = One x
    mappend (One x) (One Nothing) = One x
    mappend _ _ = error "One cannot be used twice"

data Object = Object
	{ _objId :: ObjectId
	, _objName :: String
	} deriving (Show, Eq, Ord)
makeLenses ''Object

data Door = Door
	{ doorId :: DoorId
	, doorOpen :: Bool
	} deriving (Show, Eq)

tuple :: Iso' (V2 Float) (Float, Float)
tuple = iso (\(V2 x y) -> (x, y)) (uncurry V2)
 
data Animation = Animation
    { _animId :: Int -- local id for now TODO
    , _animTileName :: String
    , _animTime :: Float
    , _animNext :: Animation
    , _animCurrentTime :: Float
    }
makeLenses ''Animation

instance Show Animation where
	show anim = show (anim^.animTileName) ++ " / " ++ show (anim^.animTime) ++ " / " ++ show (anim ^.animCurrentTime)

instance Eq Animation where
	a1 == a2 = (a1^.animId) == (a2^.animId)

-- clockwise
playerBoundary :: [(Float, Float)]
playerBoundary =
    [ (10, -90)
    , (10, -60)
    , (40, -60)
    , (40, -90)
    ]

arrowBoundary :: Orientation -> [(Float, Float)]
arrowBoundary West =
    [ (8, -28-7)
    , (8+21, -28)
    , (8+21, -28)
    , (8, -28-7)
    ]
arrowBoundary _ = arrowBoundary West

data Orientation = 
      North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest
    deriving (Eq)

toInt :: Orientation -> Int
toInt North = 0
toInt NorthEast = 1
toInt East = 2
toInt SouthEast = 3
toInt South = 4
toInt SouthWest = 5
toInt West = 6
toInt NorthWest = 7

instance Show Orientation where
    show North = "N"
    show NorthEast = "NE"
    show East = "E"
    show SouthEast = "SE"
    show South = "S"
    show SouthWest = "SW"
    show West = "W"
    show NorthWest = "NW"

deltaFromOrientation :: Orientation -> (Float, Float)
deltaFromOrientation North = (0, -1)
deltaFromOrientation NorthEast = (0.7071, -0.7071)
deltaFromOrientation East = (1, 0)
deltaFromOrientation SouthEast = (0.7071, 0.7071)
deltaFromOrientation South = (0, 1)
deltaFromOrientation SouthWest = (-0.7071, 0.7071)
deltaFromOrientation West = (-1, 0)
deltaFromOrientation NorthWest = (-0.7071, -0.7071)

orientationFromDelta :: (Float, Float) -> Orientation
orientationFromDelta (dx, dy) 
    | dx >= 2 * abs dy = East 
    | -dx >= 2 * abs dy = West
    | -dy >= 2 * abs dx = North
    | dy >= 2 * abs dx = South
    | dx >= 0 && dy <= 0 = NorthEast
    | dx >= 0 && dy >= 0 = SouthEast
    | dx <= 0 && dy <= 0 = NorthWest
    | dx <= 0 && dy >= 0 = SouthWest
    | otherwise = traceShow "orientationFromDelta error" $ error "orientationFromDelta"
    

objectAnimation :: ObjectId -> Orientation -> Animation
objectAnimation pId NorthWest = objectAnimation pId North
objectAnimation pId NorthEast = objectAnimation pId North
objectAnimation pId SouthEast = objectAnimation pId South
objectAnimation pId SouthWest = objectAnimation pId South

objectAnimation playerId dir = a1
    where
        animName num = "Player" ++ show playerId ++ show dir ++ show num
        a1 = Animation (playerId*toInt dir) (animName 1) 0.25 a2 0 
        a2 = Animation (playerId*toInt dir) (animName 2) 0.25 a3 0
        a3 = Animation (playerId*toInt dir) (animName 3) 0.25 a4 0
        a4 = Animation (playerId*toInt dir) (animName 4) 0.25 a1 0

arrowAnimation :: Orientation -> Animation
arrowAnimation dir = let a1 = Animation 99 ("Arrow" ++ show dir) 999 a1 0 in a1

