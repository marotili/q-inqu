{-# LANGUAGE TemplateHaskell, Rank2Types, NamedFieldPuns #-}
module Game.Render.World
  (
    RenderWorld(..)
  , emptyRenderWorld
  ) where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map
import Game.World.Objects(ObjectId)
import Game.Render.SpriteManager(Sprite)

import Control.Lens

type LayerName = String
data RenderObject = RenderObject
  { _roEntityId :: ObjectId
  , _roSprite :: Sprite
  }

data Layer =
  StaticLayer [RenderObject]
  | DynamicLayer (Map.Map ObjectId RenderObject)

data RenderWorld = RenderWorld
  { _rwLayers :: Map.Map LayerName Layer
  }

emptyRenderWorld :: RenderWorld
emptyRenderWorld = RenderWorld Map.empty