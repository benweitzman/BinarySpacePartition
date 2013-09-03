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
                                          , BSP(..)
                                          , Rotate(..)
                                          , ResizeDirectional(..)
                                          , Bound(..)
                                          , Swap(..)
                                          ) where

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import qualified Data.Map as M
import Data.List ((\\))
import Control.Monad

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

data BSP = EmptyBSP | Leaf | Split { left :: BSP
                                   , right :: BSP
                                   , direction :: Direction
                                   , ratio :: Rational
                                   } deriving (Show, Read)

size :: BSP -> Int
size EmptyBSP = 0
size Leaf = 1
size (Split l r _ _) = size l + size r

index :: W.Stack a -> Int
index s = case toIndex (Just s) of 
            (_, Nothing) -> 0
            (_, Just int) -> int
                       
data BinarySpacePartition a = BinarySpacePartition BSP deriving (Show, Read)

instance LayoutClass BinarySpacePartition a where
  doLayout cur@(BinarySpacePartition bsp) r s = return (zip ws rs, layout) where
    ws = W.integrate s
    count = size bsp
    layout = if l == count then Just (cur)
             else if l > count then Just (BinarySpacePartition (splitNth bsp n))
                  else  Just (BinarySpacePartition (removeNth bsp n))
    l = length ws
    n = case toIndex (Just s) of
      (_, Nothing) -> 0
      (_, Just int) -> int
    rs = case layout of
      Nothing -> rectangles bsp r
      (Just (BinarySpacePartition bsp')) -> rectangles bsp' r
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

split :: Direction -> Rational -> Rectangle -> (Rectangle, Rectangle)
split Horizontal r (Rectangle sx sy sw sh) = (r1, r2) where 
    r1 = Rectangle sx sy sw sh'
    r2 = Rectangle sx (sy + fromIntegral sh') sw (sh - sh')
    sh' = floor $ fromIntegral sh * r
split Vertical r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw' sh
    r2 = Rectangle (sx + fromIntegral sw') sy (sw - sw') sh
    sw' = floor $ fromIntegral sw * r

rectangles :: BSP -> Rectangle -> [Rectangle]
rectangles EmptyBSP _ = []
rectangles Leaf rootRect = [rootRect]
rectangles (Split l r d ra) rootRect = 
    rectangles l leftBox ++ rectangles r rightBox where
    (leftBox, rightBox) = split d ra rootRect

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
                           

applyToNth :: forall a. BSPAction a -> BSPMerge a -> BSP -> Int -> a
applyToNth action _ EmptyBSP _ = emptyAction action
applyToNth action _ parent@(Split Leaf _ _ _) 0 = leftLeafAction action $ parent
applyToNth action _ parent@(Split l Leaf _ _) n | size l <= n = rightLeafAction action $ parent
applyToNth action _ Leaf _ = leafAction action
applyToNth action merge parent@(Split l r _ _) n = if size l > n
                                     then (leftMerge merge) parent (applyToNth action merge l n)
                                     else (rightMerge merge) parent (applyToNth action merge r n)

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
                          bubbleMerge