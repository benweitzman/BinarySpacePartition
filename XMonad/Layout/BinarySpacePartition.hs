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
 
module XMonad.Layout.BinarySpacePartition (
    BinarySpacePartition,
    emptyBSP,
    ) where

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import qualified Data.Map as M
import Data.List ((\\))
import Control.Monad
import Data.Maybe

data Rotate = Rotate deriving Typeable
instance Message Rotate

data Bound = East | West | North | South deriving Typeable

data ResizeDirectional = ExpandTowards Bound | ShrinkFrom Bound deriving Typeable
instance Message ResizeDirectional

data Swap = Swap deriving Typeable
instance Message Swap


data Direction = Horizontal | Vertical deriving (Show, Read)

opposite :: Direction -> Direction
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
                   } deriving (Show, Read)
                              
reverseDirection :: Split -> Split                              
reverseDirection (Split d r) = Split (opposite d) r

data Tree a = Leaf | Node { value :: a
                          , left :: Tree a
                          , right :: Tree a 
                          } deriving (Show, Read)

leaf :: Tree a -> Bool
leaf Leaf = True
leaf _ = False

numLeaves :: Tree a -> Int
numLeaves Leaf = 1
numLeaves (Node _ l r) = numLeaves l + numLeaves r

type BSP = Tree Split

data BSPCrumb = LeftCrumb Split BSP | RightCrumb Split BSP deriving (Show, Read)

parentSplit :: BSPCrumb -> Split
parentSplit (LeftCrumb s _) = s
parentSplit (RightCrumb s _) = s

type BSPZipper = (BSP, [BSPCrumb])

bspToZipper :: BSP -> BSPZipper
bspToZipper t = (t, [])

goLeft :: BSPZipper -> Maybe BSPZipper
goLeft (Leaf, _) = Nothing
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)

goRight :: BSPZipper -> Maybe BSPZipper
goRight (Leaf, _) = Nothing
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)

goUp :: BSPZipper -> Maybe BSPZipper
goUp (_, []) = Nothing
goUp (bsp, LeftCrumb x r:bs) = Just (Node x bsp r, bs)
goUp (bsp, RightCrumb x l:bs) = Just (Node x l bsp, bs)

modify :: (Split -> Split) -> BSPZipper -> Maybe BSPZipper
modify _ (Leaf, bs) = Just (Leaf, bs)
modify f (Node x l r, bs) = Just (Node (f x) l r, bs)

goToNthLeaf :: Int -> BSPZipper -> Maybe BSPZipper
goToNthLeaf _ zip@(Leaf, _) = Just zip
goToNthLeaf n zip@(t, crumbs) = 
  if numLeaves (left t) > n
  then do zip' <- goLeft zip
          goToNthLeaf n zip'
  else do zip' <- goRight zip
          goToNthLeaf (n - (numLeaves . left $ t)) zip'
          
splitCurrentLeaf :: BSPZipper -> Maybe BSPZipper                  
splitCurrentLeaf (Leaf, []) = Just ((Node (Split Vertical 0.5) Leaf Leaf), [])
splitCurrentLeaf (Leaf, crumb:cs) = Just ((Node (reverseDirection . parentSplit $ crumb) Leaf Leaf), crumb:cs)
splitCurrentLeaf _ = Nothing

removeCurrentLeaf :: BSPZipper -> Maybe BSPZipper
removeCurrentLeaf (Leaf, []) = Nothing
removeCurrentLeaf (Leaf, (LeftCrumb x r):cs) = Just (r, cs)
removeCurrentLeaf (Leaf, (RightCrumb x l):cs) = Just (l, cs)
removeCurrentLeaf _ = Nothing

top :: BSPZipper -> BSPZipper
top z = case (goUp z) of
          Nothing -> z
          Just (z') -> top z'
          
index :: W.Stack a -> Int
index s = case toIndex (Just s) of 
            (_, Nothing) -> 0
            (_, Just int) -> int
                       
data BinarySpacePartition a = BinarySpacePartition { bsp :: Maybe BSP } deriving (Show, Read)

emptyBSP :: BinarySpacePartition a
emptyBSP = BinarySpacePartition Nothing

makeBSP :: BSP -> BinarySpacePartition a
makeBSP = BinarySpacePartition . Just

makeZipper :: BinarySpacePartition a -> Maybe BSPZipper
makeZipper (BinarySpacePartition Nothing) = Nothing
makeZipper (BinarySpacePartition (Just t)) = Just . bspToZipper $ t

zipperToBSP :: Maybe BSPZipper -> BinarySpacePartition a
zipperToBSP Nothing = BinarySpacePartition Nothing
zipperToBSP (Just zipper) = makeBSP t where (t, crumbs) = top zipper

size :: BinarySpacePartition a -> Int
size = fromMaybe 0 . fmap numLeaves . bsp

rectangles :: BinarySpacePartition a -> Rectangle -> [Rectangle]
rectangles (BinarySpacePartition Nothing) _ = []
rectangles (BinarySpacePartition (Just Leaf)) rootRect = [rootRect]
rectangles (BinarySpacePartition (Just node)) rootRect = 
    rectangles (makeBSP . left $ node) leftBox ++ 
    rectangles (makeBSP . right $ node) rightBox 
    where (leftBox, rightBox) = split (direction info) (ratio info) rootRect
          info = value node

splitNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
splitNth (BinarySpacePartition Nothing) _ = makeBSP Leaf
splitNth (BinarySpacePartition (Just Leaf)) _ = makeBSP $ Node (Split Vertical 0.5)  Leaf Leaf
splitNth b@(BinarySpacePartition (Just node)) n = zipperToBSP zipper 
  where zipper = do zip <- makeZipper b
                    zip' <- (goToNthLeaf n) zip
                    zip'' <- splitCurrentLeaf zip'
                    return $ top zip'' 
                    
removeNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a                    
removeNth b@(BinarySpacePartition Nothing) _ = emptyBSP
removeNth (BinarySpacePartition (Just Leaf)) _ = emptyBSP
removeNth b@(BinarySpacePartition (Just node)) n = zipperToBSP zipper
  where zipper = do zip <- makeZipper b
                    zip' <- (goToNthLeaf n) zip
                    zip'' <- removeCurrentLeaf zip'
                    return $ top zip''

instance LayoutClass BinarySpacePartition a where
  doLayout bsp r s = return (zip ws rs, layout) where
    ws = W.integrate s
    count = size bsp
    layout = if l == count then Nothing
             else Just $ splitNth bsp n
                  --else  Just (BinarySpacePartition (removeNth bsp n))
    l = length ws
    n = index s    
    rs = case layout of
      Nothing -> rectangles bsp r
      (Just bsp') -> rectangles bsp' r
{-
  handleMessage (BinarySpacePartition bsp) m =
    do ms <- (W.stack . W.workspace . W.current) `fmap` gets windowset
       fs <- (M.keys . W.floating) `fmap` gets windowset
       return $ ms >>= unfloat fs >>= handleMesg
    where handleMesg s = msum [fmap (\x -> rotate x s) (fromMessage m)
                              --,fmap (\x -> resize x s) (fromMessage m)
                              ,fmap (\x -> swap x s) (fromMessage m)
                              ]
          unfloat fs s = if W.focus s `elem` fs
                         then Nothing
                         else Just (s { W.up = (W.up s) \\ fs
                                      , W.down = (W.down s) \\ fs })
          rotate Rotate s = BinarySpacePartition $ rotateNth bsp $ index s
--          resize (ExpandTowards East) s = BinarySpacePartition $ rightGrowNth bsp $ index s
          swap Swap s = BinarySpacePartition $ swapNth bsp $ index s
  description _  = "BSP"


{-rightMostSplit :: BSP -> BSP
rightMostSplit EmptyBSP = Leaf
rightMostSplit (Split l Leaf d r) = 
    Split l (Split Leaf Leaf (opposite d) 0.5) d r
rightMostSplit Leaf = Split Leaf Leaf Vertical  0.5
rightMostSplit (Split l r d ra) = Split l (rightMostSplit r) d ra-}


data BSPMerge a = BSPMerge { leftMerge :: BSP -> a -> a
                            , rightMerge :: BSP -> a -> a
                            }
                  
data BSPAction a = BSPAction { emptyAction :: a
                             , leafAction :: a
                             , leftLeafAction :: BSP -> a
                             , rightLeafAction :: BSP -> a
                             }
                   
constMerge :: BSPMerge a                   
constMerge = BSPMerge (\_ v -> v) (\_ v -> v)

bubbleMerge :: BSPMerge BSP
bubbleMerge = BSPMerge (\parent child -> case parent of
                           (Split l r d ra) -> Split child r d ra
                           _ -> error "Impossible")
                       (\parent child -> case parent of
                           (Split l r d ra) -> Split l child d ra
                           _ -> error "Impossible")
                           


rightLeaf, leftLeaf :: BSP -> Int -> Bool
rightLeaf = applyToNth (BSPAction False True (const False) (const True)) constMerge
leftLeaf = applyToNth (BSPAction False True (const True) (const False)) constMerge

splitNth :: BSP -> Int -> BSP
splitNth = applyToNth (BSPAction Leaf 
                                 (Split Leaf Leaf Vertical 0.5) 
                                 (\x -> case x of
                                     (Split Leaf r d ra) -> Split (Split Leaf Leaf (opposite d) 0.5) r d ra
                                     _ -> error "Impossible")
                                 (\x -> case x of 
                                     (Split l Leaf d ra) -> Split l (Split Leaf Leaf (opposite d) 0.5) d ra
                                     _ -> error "Impossible"))
                      bubbleMerge

removeNth :: BSP -> Int -> BSP
removeNth = applyToNth (BSPAction EmptyBSP 
                                  EmptyBSP
                                  (\x -> case x of 
                                      (Split Leaf r _ _) -> r
                                      _ -> error "Impossible")
                                  (\x -> case x of 
                                      (Split l Leaf _ _) -> l
                                      _ -> error "Impossible"))
                       bubbleMerge

rotateNth :: BSP -> Int -> BSP
rotateNth = applyToNth (BSPAction EmptyBSP
                                  Leaf
                                  rotate
                                  rotate)
                       bubbleMerge
                                  
  where rotate = (\x -> case x of 
                     (Split l r d ra) -> Split l r (opposite d) ra
                     _ -> error "Impossible")
                 
swapNth :: BSP -> Int -> BSP                 
swapNth = applyToNth (BSPAction EmptyBSP
                                Leaf
                                swap
                                swap)
                     bubbleMerge
  where swap = (\x -> case x of
                   (Split l r d ra) -> Split r l d ra
                   _ -> error "Impossible")
                 
replaceNthWith_, replaceNthWith :: BSP -> BSP -> Int -> BSP
replaceNthWith_ replacement = applyToNth (BSPAction replacement
                                                    replacement
                                                    (const replacement)
                                                    (const replacement))
                                         bubbleMerge
replaceNthWith root replacement = replaceNthWith_ replacement root                                                    
                 
rightGrowNth :: BSP -> Int -> BSP
rightGrowNth = applyToNth (BSPAction EmptyBSP
                                     Leaf
                                     (\x -> case x of 
                                         (Split Leaf r Vertical ra) -> Split Leaf r Vertical $ min 0.9 (ra + 0.1)
                                         (Split Leaf r Horizontal ra) -> error "Not implemented"
                                         _ -> error "Impossible")
                                     (\x -> error "Not implemented"))
                          bubbleMerge -}

