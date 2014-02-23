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

type ObjectIds = Set.Set ObjectId
type ObjectId = Int

type DoorId = ObjectId
type DoorControllerId = ObjectId
type SwitchId = ObjectId
type PlayerId = ObjectId
type WallId = ObjectId
type BoulderId = ObjectId

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
objectAnimation 1 NorthWest = objectAnimation 1 North
objectAnimation 1 NorthEast = objectAnimation 1 North
objectAnimation 1 SouthEast = objectAnimation 1 South
objectAnimation 1 SouthWest = objectAnimation 1 South
objectAnimation 1 South = a1
    where
        a1 = Animation 1 "PlayerS1" 0.25 a2 0 
        a2 = Animation 1 "PlayerS1" 0.25 a3 0
        a3 = Animation 1 "PlayerS1" 0.25 a4 0
        a4 = Animation 1 "PlayerS1" 0.25 a1 0

--objectAnimation 1 North = b1
--    where
--        b1 = Animation 2 81 0.25 b2 0
--        b2 = Animation 2 82 0.25 b3 0
--        b3 = Animation 2 83 0.25 b4 0
--        b4 = Animation 2 84 0.25 b1 0

--objectAnimation 1 West = c1
--    where
--        c1 = Animation 3 89 0.25 c2 0
--        c2 = Animation 3 90 0.25 c3 0
--        c3 = Animation 3 91 0.25 c4 0
--        c4 = Animation 3 92 0.25 c1 0

--objectAnimation 1 East = d1
--    where
--        d1 = Animation 4 101 0.25 d2 0
--        d2 = Animation 4 102 0.25 d3 0
--        d3 = Animation 4 103 0.25 d4 0
--        d4 = Animation 4 104 0.25 d1 0

--objectAnimation 2 NorthWest = objectAnimation 2 North
--objectAnimation 2 NorthEast = objectAnimation 2 North
--objectAnimation 2 SouthEast = objectAnimation 2 South
--objectAnimation 2 SouthWest = objectAnimation 2 South
--objectAnimation 2 South = a1
--    where
--        a1 = Animation 7 137 0.25 a2 0 
--        a2 = Animation 7 138 0.25 a3 0
--        a3 = Animation 7 139 0.25 a4 0
--        a4 = Animation 7 140 0.25 a1 0

--objectAnimation 2 North = b1
--    where
--        b1 = Animation 8 141 0.25 b2 0
--        b2 = Animation 8 142 0.25 b3 0
--        b3 = Animation 8 143 0.25 b4 0
--        b4 = Animation 8 144 0.25 b1 0

--objectAnimation 2 West = c1
--    where
--        c1 = Animation 9 145 0.25 c2 0
--        c2 = Animation 9 146 0.25 c3 0
--        c3 = Animation 9 147 0.25 c4 0
--        c4 = Animation 9 148 0.25 c1 0

--objectAnimation 2 East = d1
--    where
--        d1 = Animation 10 149 0.25 d2 0
--        d2 = Animation 10 150 0.25 d3 0
--        d3 = Animation 10 151 0.25 d4 0
--        d4 = Animation 10 152 0.25 d1 0

--objectAnimation 3 _ = a1
--    where
--        a1 = Animation 5 105 2 a2 0 
--        a2 = Animation 5 106 0.1 a3 0
--        a3 = Animation 5 107 0.1 a4 0
--        a4 = Animation 5 108 0.1 a1 0

--objectAnimation 4 _ = a1
--    where
--        a1 = Animation 6 109 2 a2 0 
--        a2 = Animation 6 110 0.1 a3 0
--        a3 = Animation 6 111 0.1 a4 0
--        a4 = Animation 6 112 0.1 a1 0

objectAnimation _ _ = let a1 = Animation 1 "PlayerS1" 0.25 a1 0 in a1


arrowAnimation :: Orientation -> Animation
arrowAnimation dir = let a1 = Animation 99 ("Arrow" ++ show dir) 999 a1 0 in a1


--arrowAnimation NorthWest = let a1 = Animation 99 114 999 a1 0 in a1
--arrowAnimation North = let a1 = Animation 99 115 999 a1 0 in a1
--arrowAnimation NorthEast = let a1 = Animation 99 116 999 a1 0 in a1
--arrowAnimation East = let a1 = Animation 99 117 999 a1 0 in a1
--arrowAnimation SouthEast = let a1 = Animation 99 118 999 a1 0 in a1
--arrowAnimation South = let a1 = Animation 99 119 999 a1 0 in a1
--arrowAnimation SouthWest = let a1 = Animation 99 120 999 a1 0 in a1
