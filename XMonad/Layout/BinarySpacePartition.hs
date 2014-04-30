{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BinarySpacePartition
-- Copyright   :  (c) 2013 Ben Weitzman <benweitzman@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ben Weitzman <benweitzman@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout where new windows will split the focused window in half.
--
-----------------------------------------------------------------------------
 
module XMonad.Layout.BinarySpacePartition (BinarySpacePartition(..)
                                          , emptyBSP
                                          , Rotate(..)
                                          , Swap(..)
                                          , ResizeDirectional(..)
                                          , Bound(..)
                                          ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Stack hiding (Zipper)
import qualified Data.Map as M
import Data.List ((\\))
import Control.Monad
import Data.Maybe

class Negatable a where
  opposite :: a -> a

data Rotate = Rotate deriving Typeable
instance Message Rotate

data Bound = East | West | North | South deriving Typeable

instance Negatable Bound where
  opposite East = West
  opposite West = East
  opposite North = South
  opposite South = North

data ResizeDirectional = ExpandTowards Bound | ShrinkFrom Bound deriving Typeable
instance Message ResizeDirectional

data Swap = Swap deriving Typeable
instance Message Swap

data Direction = Horizontal | Vertical deriving (Show, Read, Eq)

instance Negatable Direction where
  opposite Vertical = Horizontal
  opposite Horizontal = Vertical

split :: Direction -> Rational -> Rectangle -> (Rectangle, Rectangle)
split Horizontal r (Rectangle sx sy sw sh) = (r1, r2) where 
    r1 = Rectangle sx sy sw sh'
    r2 = Rectangle sx (sy + fromIntegral sh') sw (sh - sh')
    sh' = floor $ fromIntegral sh * r
split Vertical r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw' sh
    r2 = Rectangle (sx + fromIntegral sw') sy (sw - sw') sh
    sw' = floor $ fromIntegral sw * r

data Split = Split { direction :: Direction
                   , ratio :: Rational
                   } deriving (Show, Read, Eq)
                              
oppositeDirection :: Split -> Split
oppositeDirection (Split d r) = Split (opposite d) r

increaseRatio :: Split -> Rational -> Split
increaseRatio (Split d r) delta = Split d (min 0.9 (max 0.1 (r + delta))) 

data Tree a = Leaf | Node { value :: a
                          , left :: Tree a
                          , right :: Tree a 
                          } deriving (Show, Read, Eq)

numLeaves :: Tree a -> Int
numLeaves Leaf = 1
numLeaves (Node _ l r) = numLeaves l + numLeaves r

type BSP = Tree Split

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show, Read, Eq)

swapCrumb :: Crumb a -> Crumb a
swapCrumb (LeftCrumb s t) = RightCrumb s t
swapCrumb (RightCrumb s t) = LeftCrumb s t

parentVal :: Crumb a -> a
parentVal (LeftCrumb s _) = s
parentVal (RightCrumb s _) = s

modifyParentVal :: (a -> a) -> Crumb a -> Crumb a
modifyParentVal f (LeftCrumb s t) = LeftCrumb (f s) t
modifyParentVal f (RightCrumb s t) = RightCrumb (f s) t

type Zipper a = (Tree a, [Crumb a])

toZipper :: Tree a -> Zipper a
toZipper t = (t, [])

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Leaf, _) = Nothing
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Leaf, _) = Nothing
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (t, LeftCrumb x r:cs) = Just (Node x t r, cs)
goUp (t, RightCrumb x l:cs) = Just (Node x l t, cs)

goSibling :: Zipper a -> Maybe (Zipper a)
goSibling (_, []) = Nothing
goSibling z@(_, LeftCrumb _ _:_) = Just z >>= goUp >>= goRight
goSibling z@(_, RightCrumb _ _:_) = Just z >>= goUp >>= goLeft

goToNthLeaf :: Int -> Zipper a -> Maybe (Zipper a)
goToNthLeaf _ z@(Leaf, _) = Just z
goToNthLeaf n z@(t, _) = 
  if numLeaves (left t) > n
  then do z' <- goLeft z
          goToNthLeaf n z'
  else do z' <- goRight z
          goToNthLeaf (n - (numLeaves . left $ t)) z'
          
splitCurrentLeaf :: Zipper Split -> Maybe (Zipper Split)                  
splitCurrentLeaf (Leaf, []) = Just ((Node (Split Vertical 0.5) Leaf Leaf), [])
splitCurrentLeaf (Leaf, crumb:cs) = Just ((Node (Split (opposite . direction . parentVal $ crumb) 0.5) Leaf Leaf), crumb:cs)
splitCurrentLeaf _ = Nothing

removeCurrentLeaf :: Zipper a -> Maybe (Zipper a)
removeCurrentLeaf (Leaf, []) = Nothing
removeCurrentLeaf (Leaf, (LeftCrumb _ r):cs) = Just (r, cs)
removeCurrentLeaf (Leaf, (RightCrumb _ l):cs) = Just (l, cs)
removeCurrentLeaf _ = Nothing

rotateCurrentLeaf :: Zipper Split -> Maybe (Zipper Split)
rotateCurrentLeaf (Leaf, []) = Just (Leaf, [])
rotateCurrentLeaf (Leaf, c:cs) = Just (Leaf, modifyParentVal oppositeDirection c:cs)
rotateCurrentLeaf _ = Nothing

swapCurrentLeaf :: Zipper a -> Maybe (Zipper a)
swapCurrentLeaf (Leaf, []) = Just (Leaf, [])
swapCurrentLeaf (Leaf, c:cs) = Just (Leaf, swapCrumb c:cs) 
swapCurrentLeaf _ = Nothing

expandTreeTowards :: Bound -> Zipper Split -> Maybe (Zipper Split)
expandTreeTowards _ z@(_, []) = Just z
expandTreeTowards East (t, LeftCrumb s r:cs) 
  | direction s == Vertical = Just (t, LeftCrumb (increaseRatio s 0.1) r:cs)
expandTreeTowards West (t, RightCrumb s l:cs)                              
  | direction s == Vertical = Just (t, RightCrumb (increaseRatio s (-0.1)) l:cs)
expandTreeTowards South (t, LeftCrumb s r:cs)                              
  | direction s == Horizontal = Just (t, LeftCrumb (increaseRatio s 0.1) r:cs)
expandTreeTowards North (t, RightCrumb s l:cs)                                 
  | direction s == Horizontal = Just (t, RightCrumb (increaseRatio s (-0.1)) l:cs)
expandTreeTowards dir z = do z' <- goUp z                                
                             expandTreeTowards dir z'
                             
shrinkTreeFrom :: Bound -> Zipper Split -> Maybe (Zipper Split)                          
shrinkTreeFrom _ z@(_, []) = Just z
shrinkTreeFrom dir z = Just z >>= goSibling >>= (expandTreeTowards . opposite $ dir)
                              
top :: Zipper a -> Zipper a
top z = case (goUp z) of
          Nothing -> z
          Just (z') -> top z'

toTree :: Zipper a -> Tree a
toTree = fst . top

index :: W.Stack a -> Int
index s = case toIndex (Just s) of 
            (_, Nothing) -> 0
            (_, Just int) -> int
                       
data BinarySpacePartition a = BinarySpacePartition { getTree :: Maybe (Tree Split) } deriving (Show, Read)

emptyBSP :: BinarySpacePartition a
emptyBSP = BinarySpacePartition Nothing

makeBSP :: Tree Split -> BinarySpacePartition a
makeBSP = BinarySpacePartition . Just

makeZipper :: BinarySpacePartition a -> Maybe (Zipper Split)
makeZipper (BinarySpacePartition Nothing) = Nothing
makeZipper (BinarySpacePartition (Just t)) = Just . toZipper $ t

size :: BinarySpacePartition a -> Int
size = fromMaybe 0 . fmap numLeaves . getTree

zipperToBinarySpacePartition :: Maybe (Zipper Split) -> BinarySpacePartition b
zipperToBinarySpacePartition Nothing = BinarySpacePartition Nothing
zipperToBinarySpacePartition (Just z) = BinarySpacePartition . Just . toTree . top $ z

rectangles :: BinarySpacePartition a -> Rectangle -> [Rectangle]
rectangles (BinarySpacePartition Nothing) _ = []
rectangles (BinarySpacePartition (Just Leaf)) rootRect = [rootRect]
rectangles (BinarySpacePartition (Just node)) rootRect = 
    rectangles (makeBSP . left $ node) leftBox ++ 
    rectangles (makeBSP . right $ node) rightBox 
    where (leftBox, rightBox) = split (direction info) (ratio info) rootRect
          info = value node

doToNth :: (Zipper Split -> Maybe (Zipper Split)) -> BinarySpacePartition a -> Int -> BinarySpacePartition a
doToNth f b n = zipperToBinarySpacePartition $ makeZipper b >>= goToNthLeaf n >>= f

splitNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
splitNth (BinarySpacePartition Nothing) _ = makeBSP Leaf
splitNth b n = doToNth splitCurrentLeaf b n 

removeNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a                    
removeNth (BinarySpacePartition Nothing) _ = emptyBSP
removeNth (BinarySpacePartition (Just Leaf)) _ = emptyBSP
removeNth b n = doToNth removeCurrentLeaf b n 
                    
rotateNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a                    
rotateNth (BinarySpacePartition Nothing) _ = emptyBSP
rotateNth b@(BinarySpacePartition (Just Leaf)) _ = b
rotateNth b n = doToNth rotateCurrentLeaf b n 
                                        
swapNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
swapNth (BinarySpacePartition Nothing) _ = emptyBSP
swapNth b@(BinarySpacePartition (Just Leaf)) _ = b
swapNth b n = doToNth swapCurrentLeaf b n 
                    
growNthTowards :: Bound -> BinarySpacePartition a -> Int -> BinarySpacePartition a
growNthTowards _ (BinarySpacePartition Nothing) _ = emptyBSP
growNthTowards _ b@(BinarySpacePartition (Just Leaf)) _ = b
growNthTowards dir b n = doToNth (expandTreeTowards dir) b n 
                    
shrinkNthFrom :: Bound -> BinarySpacePartition a -> Int -> BinarySpacePartition a                    
shrinkNthFrom _ (BinarySpacePartition Nothing) _ = emptyBSP
shrinkNthFrom _ b@(BinarySpacePartition (Just Leaf)) _ = b
shrinkNthFrom dir b n = doToNth (shrinkTreeFrom dir) b n 

instance LayoutClass BinarySpacePartition a where
  doLayout b r s = return (zip ws rs, layout) where
    ws = W.integrate s
    count = size b
    layout = if l == count 
             then Just b
             else if l > count  
                  then Just $ splitNth b n
                  else  Just $ removeNth b n
    l = length ws
    n = index s    
    rs = case layout of
      Nothing -> rectangles b r
      Just bsp' -> rectangles bsp' r
  handleMessage b m =
    do ms <- (W.stack . W.workspace . W.current) `fmap` gets windowset
       fs <- (M.keys . W.floating) `fmap` gets windowset
       return $ ms >>= unfloat fs >>= handleMesg
    where handleMesg s = msum [fmap (\x -> rotate x s) (fromMessage m)
                              ,fmap (\x -> resize x s) (fromMessage m)
                              ,fmap (\x -> swap x s) (fromMessage m)
                              ]
          unfloat fs s = if W.focus s `elem` fs
                         then Nothing
                         else Just (s { W.up = (W.up s) \\ fs
                                      , W.down = (W.down s) \\ fs })
          rotate Rotate s = rotateNth b $ index s
          swap Swap s = swapNth b $ index s
          resize (ExpandTowards dir) s = growNthTowards dir b $ index s
          resize (ShrinkFrom dir) s = shrinkNthFrom dir b $ index s
  description _  = "BSP"

