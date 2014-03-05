{-# LANGUAGE TemplateHaskell #-}
module Game.Data.Bin where

import Control.Lens
import Data.Monoid
import qualified Data.Map as Map

type RectId = Int
type Position = (Int, Int)

data Rect = Rect 
	{ _rectId :: RectId
	, _rectWidth :: Int
	, _rectHeight :: Int
	} deriving (Show, Eq)

newRect rId w h = Rect rId w h

data Atlas = Atlas
	{ _atlasRects :: Map.Map RectId Position
	} deriving (Show)


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

--instance Show PackTree where
--	show (PackHorizontalNode l r) = "PackHorizontalNode " ++ show l ++ " / " ++ show r
--	show (PackVerticalNode l r) = "PackVerticalNode" ++ show l ++ " / " ++ show r
--	show (PackLeafEmpty w h)  = "PackLeafEmpty " ++ show w ++ " " ++ show h
--	show (PackLeafRect rId) = "PackLeafRect " ++ show rId

data Bin = Bin
	{ _binWidth :: Int
	, _binHeight :: Int
	} deriving (Show)

makeLenses ''Rect
makeLenses ''PackTree
makeLenses ''Atlas
makeLenses ''Bin

instance Ord Rect where
	r1 <= r2 = if r1^.rectWidth <= r2^.rectWidth then
		True else False

packTree :: Getter PackTree [PackTree]
packTree = let
		get n@(PackHorizontalNode l r) = n : (l^.packTree ++ r^.packTree)
		get n@(PackVerticalNode l r) = n : (l^.packTree ++ r^.packTree)
		get n@(PackLeafEmpty _ _ _ _) = n : []
		get n@(PackLeafRect _ _ _) = n : []
	in to get

atlasFromTree pt = Atlas $ foldr (\pt m -> case pt of
		PackLeafRect rId w h -> Map.insert rId (w, h) m
		_ -> m
	) Map.empty (pt^.packTree)

foldTree :: Rect -> PackTree -> Maybe PackTree
foldTree rect pt = unOne $ foldl (\mo tree -> case tree of
		node@(PackLeafEmpty w h _ _) -> 
			if w > (rect^.rectWidth) && h > (rect^.rectHeight)
				then mo `mappend` (One node)
				else mo `mappend` None 
		_ -> mappend mo None
	) None (pt^.packTree)

updateTree :: Rect -> PackTree -> Maybe PackTree -> Maybe PackTree
updateTree rect n@(PackHorizontalNode l r) _ = case updateTree rect l (Just n) of 
	Nothing -> case updateTree rect r (Just n) of
		Nothing -> Just n
		Just r' -> Just $ PackHorizontalNode l r'
	Just l' -> Just $ PackHorizontalNode l' r

updateTree rect n@(PackVerticalNode l r) _ = case updateTree rect l (Just n) of 
	Nothing -> case updateTree rect r (Just n) of
		Nothing -> Just n
		Just r' -> Just $ PackVerticalNode l r'
	Just l' -> Just $ PackVerticalNode l' r

updateTree rect n@(PackLeafEmpty w h x y) (Just (PackVerticalNode _ _)) = 
	if rect^.rectWidth <= w && rect^.rectHeight <= h 
		then Just $ PackHorizontalNode (
				PackVerticalNode
					(PackLeafRect (rect^.rectId) x y)
					(PackLeafEmpty (w - rect^.rectWidth) (rect^.rectHeight) (x + rect^.rectWidth) y)
				) 
				(PackLeafEmpty w (h - rect^.rectHeight) x (y + rect^.rectHeight))
		else Nothing

updateTree rect n@(PackLeafEmpty w h x y) (Just (PackHorizontalNode _ _)) = 
	if rect^.rectWidth <= w && rect^.rectHeight <= h 
		then Just $ PackVerticalNode (
				PackHorizontalNode
					(PackLeafRect (rect^.rectId) x y)
					(PackLeafEmpty (rect^.rectWidth) (h - rect^.rectHeight) x (y + rect^.rectHeight))
				) 
				(PackLeafEmpty (w - rect^.rectWidth) h (x + rect^.rectWidth) y)
		else Nothing

updateTree _ _ _ = Nothing

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

test :: IO ()
test = do
	let Just root = mkRoot (Rect 0 20 20) (Bin 1024 768)
	let Just ft = updateTree (Rect 1 15 15) root Nothing
	let ft' = updateTree (Rect 5 15 15) ft Nothing
	print ft'
--	print $ packOne (Rect 1 15 15) root

--fold :: (a -> b -> b) -> b -> [a]

data One a = One a | None deriving (Show)
unOne (One a) = Just a
unOne None = Nothing

instance Monoid (One a) where
	mempty = None
	mappend (One a) None = One a
	mappend None (One a) = One a
	mappend None None = None
	mappend (One a) _ = One a

