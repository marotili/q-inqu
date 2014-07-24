{-# LANGUAGE FlexibleInstances, NamedFieldPuns, TemplateHaskell, Rank2Types #-}
module Game.World.Objects 
    ( Animation(..)
    , orientationToInt
    , Orientation(..)
    , BaseData(..)
    , bdOrigin
    , bdBaseBoundary
    , ObjectId
    , Position
    , Object 
    , PlayerId
    , objName
    , One(..)
    , unOne
    , Object(..)

    , animNext
    , animCurrentTime
    , animTime

    , orientationFromDelta
    )
where
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

data BaseData = BaseData
    { _bdOrigin :: (Float, Float)
    , _bdBaseBoundary :: [(Float, Float)]
    }
makeLenses ''BaseData


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

orientationToInt :: Orientation -> Int
orientationToInt North = 0
orientationToInt NorthEast = 1
orientationToInt East = 2
orientationToInt SouthEast = 3
orientationToInt South = 4
orientationToInt SouthWest = 5
orientationToInt West = 6
orientationToInt NorthWest = 7

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
    