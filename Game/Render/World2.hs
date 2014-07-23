module Game.Render.World2
(
) where

type LayerName = String

data SpriteManager = SpriteManager
    { 
    }

data Layer =
    StaticLayer [EntityRenderData]
    | DynamicLayer (Map.Map EntityId EntityRenderData)

data EntityRenderData = EntityRenderData
    { _erdSpriteName :: String
    }

data RenderData = RenderData
    { _rdLayers :: Map.Map LayerName Layer
    , _rdLayerSort :: [LayerName]
    }
