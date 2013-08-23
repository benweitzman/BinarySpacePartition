{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
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
 
module XMonad.Layout.BinarySpacePartition (BinarySpacePartition(..), BSP(..)) where

import XMonad.Core
import Graphics.X11 (Rectangle(..))
import qualified XMonad.StackSet as W
import XMonad.Util.Stack


data Direction = Horizontal | Vertical deriving (Show, Read)

opposite :: Direction -> Direction
opposite Vertical = Horizontal
opposite Horizontal = Vertical

data BSP = EmptyBSP | Leaf | Split { left :: BSP
                                   , right :: BSP
                                   , direction :: Direction
                                   , ratio :: Rational
                                   } deriving (Show, Read)

data BinarySpacePartition a = BinarySpacePartition BSP deriving (Show, Read)

instance LayoutClass BinarySpacePartition a where
    doLayout (BinarySpacePartition bsp) r s = return (zip ws rs, layout) where
        ws = W.integrate s
        count = size bsp
        layout = if length ws == count then Nothing 
                 else Just (BinarySpacePartition (splitNth bsp n)) where
                        n = case toIndex (Just s) of
                              (_, Nothing) -> 0
                              (_, Just int) -> int
        rs = case layout of
                Nothing -> rectangles bsp r
                (Just (BinarySpacePartition bsp')) -> rectangles bsp' r
    pureMessage _ _ = Nothing
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

rightMostSplit :: BSP -> BSP
rightMostSplit EmptyBSP = Leaf
rightMostSplit (Split l Leaf d r) = 
    Split l (Split Leaf Leaf (opposite d) 0.5) d r
rightMostSplit Leaf = Split Leaf Leaf Vertical  0.5
rightMostSplit (Split l r d ra) = Split l (rightMostSplit r) d ra

splitNth :: BSP -> Int -> BSP
splitNth EmptyBSP _ = Leaf
splitNth (Split Leaf r d ra) 0 = Split (Split Leaf Leaf (opposite d) 0.5) r d ra
splitNth (Split l Leaf d ra) n
    | size l <= n = Split l (Split Leaf Leaf (opposite d) 0.5) d ra
splitNth Leaf _ = Split Leaf Leaf Vertical 0.5
splitNth (Split l r d ra) n = if size l >  n 
                              then Split (splitNth l n) r d ra
                              else Split l (splitNth r (n - size l)) d ra

size :: BSP -> Int
size EmptyBSP = 0
size Leaf = 1
size (Split l r _ _) = size l + size r