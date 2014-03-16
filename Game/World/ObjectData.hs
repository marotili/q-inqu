{-# LANGUAGE FlexibleInstances, NamedFieldPuns, TemplateHaskell, Rank2Types #-}
module Game.World.ObjectData where

import qualified Data.Set as Set
import Linear
import Debug.Trace
import Control.Lens
import Data.Monoid
import Game.World.Objects

--bdPosition :: (Float, Float) -> Float -> Getter BaseData (Float, Float)
bdBoundary :: Float -> Getter BaseData [(Float, Float)]
bdBoundary rotation = to get
    where
        get bd = map (\(x, y) -> rotate rotation
                ( x - (bd^.bdOrigin._1)
                , y - (bd^.bdOrigin._2)
                )
            ) (bd^.bdBaseBoundary)

        rotate a (x, y) = (cos a * x + sin a * y, - sin a * x + cos a * y)

-- clockwise
playerBoundary :: [(Float, Float)]
playerBoundary =
    [ (-15, -20)
    , (-15, 10)
    , (15, 10)
    , (15, -20)
    ]

arrowData = BaseData
    { _bdOrigin = (8, -32)
    , _bdBaseBoundary = arrowBoundary
    }

arrowBoundary :: [(Float, Float)]
arrowBoundary =
    [ (49, -28-8)
    , (55, -28)
    , (55, -28)
    , (49, -28-8)
    ]



objectAnimation :: ObjectId -> Orientation -> Animation
objectAnimation pId NorthWest = objectAnimation pId North
objectAnimation pId NorthEast = objectAnimation pId North
objectAnimation pId SouthEast = objectAnimation pId South
objectAnimation pId SouthWest = objectAnimation pId South

objectAnimation 3 dir = a1
    where
        animName num = "FWTFrontStand"
        a1 = Animation (3*toInt dir) (animName 1) 0.25 a2 0 
        a2 = Animation (3*toInt dir) (animName 2) 0.25 a3 0
        a3 = Animation (3*toInt dir) (animName 3) 0.25 a4 0
        a4 = Animation (3*toInt dir) (animName 4) 0.25 a1 0

objectAnimation 4 dir = a1
    where
        animName num = "FWTFrontStand"
        a1 = Animation (4*toInt dir) (animName 1) 0.25 a2 0 
        a2 = Animation (4*toInt dir) (animName 2) 0.25 a3 0
        a3 = Animation (4*toInt dir) (animName 3) 0.25 a4 0
        a4 = Animation (4*toInt dir) (animName 4) 0.25 a1 0

objectAnimation playerId dir = a1
    where
        animName num = "FWTFrontStand"
        a1 = Animation (playerId*toInt dir) (animName 1) 0.25 a2 0 
        a2 = Animation (playerId*toInt dir) (animName 2) 0.25 a3 0
        a3 = Animation (playerId*toInt dir) (animName 3) 0.25 a4 0
        a4 = Animation (playerId*toInt dir) (animName 4) 0.25 a1 0

arrowAnimation :: Orientation -> Animation
arrowAnimation dir = let a1 = Animation 99 ("FWTProjectile") 999 a1 0 in a1

boltAnimation = let a1 = Animation 100 "ItemBolt" 999 a1 0 in a1