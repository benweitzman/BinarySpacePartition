{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
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
 
module XMonad.Layout.BinarySpacePartition (BinarySpacePartition(..), BSP(..), Rotate(..)) where

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import qualified Data.Map as M
import Data.List ((\\))
import Control.Monad

data Rotate = Rotate deriving Typeable
instance Message Rotate

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
    where handleMesg s = msum [fmap (\x -> rotate x s) (fromMessage m)]
          unfloat fs s = if W.focus s `elem` fs
                         then Nothing
                         else Just (s { W.up = (W.up s) \\ fs
                                      , W.down = (W.down s) \\ fs })
          rotate Rotate s = BinarySpacePartition (rotateNth bsp (case toIndex (Just s) of
                                                                    (_, Nothing) -> 0
                                                                    (_, Just int) -> int))
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
    
rightLeaf :: BSP -> Int -> Bool
rightLeaf EmptyBSP _ = False
rightLeaf (Split Leaf _ _ _) 0 = False
rightLeaf (Split l Leaf _ _) n | size l <= n = True
rightLeaf Leaf _ = True
rightLeaf (Split l r _ _) n = if size l > n
                              then rightLeaf l n
                              else rightLeaf r (n - size l)

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

removeNth :: BSP -> Int -> BSP
removeNth EmptyBSP _ = EmptyBSP
removeNth (Split Leaf r _ _) 0 = r
removeNth (Split l Leaf _ _) n
  | size l <= n = l
removeNth Leaf _ = EmptyBSP
removeNth (Split l r d ra) n = if size l > n
                               then Split (removeNth l n) r d ra
                               else Split l (removeNth r (n - size l)) d ra

rotateNth :: BSP -> Int -> BSP
rotateNth EmptyBSP _ = EmptyBSP
rotateNth (Split Leaf r d ra) 0 = Split Leaf r (opposite d) ra
rotateNth (Split l Leaf d ra) n
  | size l <= n = Split l Leaf (opposite d) ra
rotateNth Leaf _ = Leaf
rotateNth (Split l r d ra) n = if size l > n
                               then Split (rotateNth l n) r d ra
                               else Split l (rotateNth r (n - size l)) d ra



size :: BSP -> Int
size EmptyBSP = 0
size Leaf = 1
size (Split l r _ _) = size l + size r