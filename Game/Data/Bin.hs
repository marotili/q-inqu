{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Game.Data.Bin
(
	newRect,
	packRects,
	atlasRect
)

where

-- | 

import Control.Lens
--import Data.Monoid
import qualified Data.Map as Map
import Data.Maybe

type RectId = Int
type Position = (Int, Int)

data Rect = Rect 
	{ _rectId :: RectId
	, _rectWidth :: Int
	, _rectHeight :: Int
	} deriving (Show, Eq)

-- | create a new axis aligned rectangle
newRect :: RectId -> Int -> Int -> Rect
newRect = Rect

data Atlas = Atlas
	{ _atlasRects :: Map.Map RectId Position
	} deriving (Show)
makeLenses ''Atlas

-- | get the position of the given rect
atlasRect :: RectId -> Getter Atlas (Maybe Position)
atlasRect rId = to (\atlas -> atlas^.atlasRects.at rId)

data PackTree =
	  PackHorizontalNode PackTree PackTree
	| PackVerticalNode PackTree PackTree
	| PackLeafEmpty 
		{ _ptEmptyWidth :: Int
		, _ptEmptyHeight :: Int
		, _ptEmptyX :: Int
		, _ptEmptyY :: Int
		}
	| PackLeafRect RectId Int Int
	deriving (Show)

data Bin = Bin
	{ _binWidth :: Int
	, _binHeight :: Int
	} deriving (Show)

-- | create a new bin with the given size
newBin :: Int -> Int -> Bin
newBin = Bin

makeLenses ''Rect
makeLenses ''Bin

-- | pack multiple rectangles into an atlas
-- | TODO: correct error handling
packRects :: (Int, Int) -> [Rect] -> Atlas
packRects _ [] = error "packRects expects non empty list"
packRects (width, height) (rect:rects) = atlasFromTree $ foldr (\r a ->
		fromMaybe (error "building bin packing tree failed") (updateTree r a Nothing)
			) (fromMaybe (error "mkroot failed") $ mkRoot rect (newBin width height)) rects

instance Ord Rect where
	r1 `compare` r2 
		| r1^.rectHeight < r2^.rectHeight = LT
		| r1^.rectHeight > r2^.rectHeight = GT
		| r1^.rectWidth < r2^.rectWidth = LT
		| r2^.rectWidth > r2^.rectWidth = GT
		| otherwise = EQ

-- | helper: flatten the tree
packTree :: Getter PackTree [PackTree]
packTree = let
		get n@(PackHorizontalNode l r) = n : (l^.packTree ++ r^.packTree)
		get n@(PackVerticalNode l r) = n : (l^.packTree ++ r^.packTree)
		get n@(PackLeafEmpty {}) = [n]
		get n@(PackLeafRect {}) = [n]
	in to get

-- | create an atlas from the pack tree
atlasFromTree :: PackTree -> Atlas
atlasFromTree pTree = Atlas $ foldr (\pt m -> case pt of
		PackLeafRect rId w h -> Map.insert rId (w, h) m
		_ -> m
	) Map.empty (pTree^.packTree)


-- | update the tree and insert a new node
updateTree :: Rect -> PackTree -> Maybe PackTree -> Maybe PackTree
updateTree rect n@(PackHorizontalNode l r) _ = case updateTree rect l (Just n) of 
	Nothing -> case updateTree rect r (Just n) of
		Nothing -> Nothing
		Just r' -> Just $ PackHorizontalNode l r'
	Just l' -> Just $ PackHorizontalNode l' r

updateTree rect n@(PackVerticalNode l r) _ = case updateTree rect l (Just n) of 
	Nothing -> case updateTree rect r (Just n) of
		Nothing -> Nothing
		Just r' -> Just $ PackVerticalNode l r'
	Just l' -> Just $ PackVerticalNode l' r

updateTree rect (PackLeafEmpty w h x y) (Just (PackVerticalNode _ _)) = 
	if rect^.rectWidth <= w && rect^.rectHeight <= h 
		then 
			Just $ if w - rect^.rectWidth == 0 then
				if h - rect^.rectHeight == 0 then
					PackLeafRect (rect^.rectId) x y
				else PackHorizontalNode 
					(PackLeafRect (rect^.rectId) x y)
					(PackLeafEmpty w (h - rect^.rectHeight) x (y + rect^.rectHeight))
			else if h - rect^.rectHeight == 0 then
				PackVerticalNode
					(PackLeafRect (rect^.rectId) x y)
					(PackLeafEmpty (w - rect^.rectWidth) (rect^.rectHeight) (x + rect^.rectWidth) y)
			else PackHorizontalNode (
				PackVerticalNode
					(PackLeafRect (rect^.rectId) x y)
					(PackLeafEmpty (w - rect^.rectWidth) (rect^.rectHeight) (x + rect^.rectWidth) y)
				) 
				(PackLeafEmpty w (h - rect^.rectHeight) x (y + rect^.rectHeight))
		else Nothing

updateTree rect (PackLeafEmpty w h x y) (Just (PackHorizontalNode _ _)) = 
	if rect^.rectWidth <= w && rect^.rectHeight <= h 
		then Just $ if w - rect^.rectWidth == 0 then
			if h - rect^.rectHeight == 0 then
				PackLeafRect (rect^.rectId) x y	
			else
				PackHorizontalNode
					(PackLeafRect (rect^.rectId) x y)
					(PackLeafEmpty (rect^.rectWidth) (h - rect^.rectHeight) x (y + rect^.rectHeight))
		else
			if h - rect^.rectHeight == 0 then
				PackVerticalNode
					(PackLeafRect (rect^.rectId) x y)
					(PackLeafEmpty (w - rect^.rectWidth) h (x + rect^.rectWidth) y)
			else
				PackVerticalNode (
					PackHorizontalNode
						(PackLeafRect (rect^.rectId) x y)
						(PackLeafEmpty (rect^.rectWidth) (h - rect^.rectHeight) x (y + rect^.rectHeight))
					) 
					(PackLeafEmpty (w - rect^.rectWidth) h (x + rect^.rectWidth) y)


		else Nothing

updateTree _ _ _ = Nothing

-- | make a new root with the first rect and a bin
mkRoot :: Rect -> Bin -> Maybe PackTree
mkRoot rect bin =
	let 
		spaceRight = bin^.binWidth - rect^.rectWidth
		spaceBottom = bin^.binHeight - rect^.rectHeight
		root = if spaceRight < 0 || spaceBottom < 0 
			then Nothing
			else Just $
				PackHorizontalNode (
					PackVerticalNode
						(PackLeafRect (rect^.rectId) 0 0)
						(PackLeafEmpty spaceRight (rect^.rectHeight) (rect^.rectWidth) 0)
					)
					(PackLeafEmpty (bin^.binWidth) spaceBottom 0 (rect^.rectHeight))
				
	in root
