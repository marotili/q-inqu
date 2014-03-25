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

objectAnimation 1 dir = a1
    where
        animName South 1 = "LilaStandFront"
        animName South 2 = "LilaWalk1Front"
        animName South 3 = "LilaStandFront"
        animName South 4 = "LilaWalk2Front"

        animName North 1 = "LilaStandBack"
        animName North 2 = "LilaWalk1Back"
        animName North 3 = "LilaStandBack"
        animName North 4 = "LilaWalk2Back"

        animName West 1 = "LilaStandLeft"
        animName West 2 = "LilaWalk1Left"
        animName West 3 = "LilaStandLeft"
        animName West 4 = "LilaWalk2Left"

        animName East 1 = "LilaStandRight"
        animName East 2 = "LilaWalk1Right"
        animName East 3 = "LilaStandRight"
        animName East 4 = "LilaWalk2Right"

        a1 = Animation (3*toInt dir) (animName dir 1) 0.2 a2 0 
        a2 = Animation (3*toInt dir) (animName dir 2) 0.2 a3 0
        a3 = Animation (3*toInt dir) (animName dir 3) 0.2 a4 0
        a4 = Animation (3*toInt dir) (animName dir 4) 0.2 a1 0

objectAnimation 3 dir = a1
    where
        animName South 1 = "WolfStandFront"
        animName South 2 = "WolfFrontWalk1"
        animName South 3 = "WolfStandFront"
        animName South 4 = "WolfFrontWalk1"

        animName North 1 = "WolfStandFront"
        animName North 2 = "WolfStandFront"
        animName North 3 = "WolfStandFront"
        animName North 4 = "WolfStandFront"

        animName West 1 = "WolfStandLeft"
        animName West 2 = "WolfWalk1Left"
        animName West 3 = "WolfStandLeft"
        animName West 4 = "WolfWalk1Left"

        animName East 1 = "WolfStandRight"
        animName East 2 = "WolfRightWalk1"
        animName East 3 = "WolfStandRight"
        animName East 4 = "WolfRightWalk1"

        a1 = Animation (4*toInt dir) (animName dir 1) 0.25 a2 0 
        a2 = Animation (4*toInt dir) (animName dir 2) 0.25 a3 0
        a3 = Animation (4*toInt dir) (animName dir 3) 0.25 a4 0
        a4 = Animation (4*toInt dir) (animName dir 4) 0.25 a1 0

objectAnimation 4 dir = a1
    where
        animName South 1 = "FWTFrontStand"
        animName South 2 = "FWTFrontWalk1"
        animName South 3 = "FWTFrontStand"
        animName South 4 = "FWTFrontWalk2"

        animName North 1 = "FWTBackStand"
        animName North 2 = "FWTBackWalk1"
        animName North 3 = "FWTBackStand"
        animName North 4 = "FWTBackWalk2"

        animName West 1 = "FWTLeftStand"
        animName West 2 = "FWTLeftWalk1"
        animName West 3 = "FWTLeftStand"
        animName West 4 = "FWTLeftWalk2"

        animName East 1 = "FWTRightStand"
        animName East 2 = "FWTRightWalk1"
        animName East 3 = "FWTRightStand"
        animName East 4 = "FWTRightWalk2"

        a1 = Animation (4*toInt dir) (animName dir 1) 0.25 a2 0 
        a2 = Animation (4*toInt dir) (animName dir 2) 0.25 a3 0
        a3 = Animation (4*toInt dir) (animName dir 3) 0.25 a4 0
        a4 = Animation (4*toInt dir) (animName dir 4) 0.25 a1 0

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