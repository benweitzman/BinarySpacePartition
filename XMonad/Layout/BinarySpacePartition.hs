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
-- Layout where new windows will split the focused window in half, based off of BSPWM
--
-----------------------------------------------------------------------------

module XMonad.Layout.BinarySpacePartition (
                                          -- * Usage
                                          -- $usage
                                            emptyBSP
                                          , Rotate(..)
                                          , Swap(..)
                                          , ResizeDirectional(..)
                                          , TreeRotate(..)
                                          , TreeFlip(..)
                                          , TreeBalance(..)
                                          , Circulate(..)
                                          , Direction2D(..)
                                          ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Stack hiding (Zipper)
import XMonad.Util.Types
import qualified Data.Map as M
import Data.List ((\\),elemIndex)
import Data.Maybe (fromMaybe, fromJust, isNothing, mapMaybe)
import Control.Monad
import Control.Applicative

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.BinarySpacePartition
--
-- Then add the layout, using the default BSP (BinarySpacePartition)
--
-- > myLayout = emptyBSP ||| etc ..
--
-- It will be helpful to add the following key bindings
--
-- > , ((modm .|. altMask,               xK_l     ), sendMessage $ ExpandTowards R)
-- > , ((modm .|. altMask,               xK_h     ), sendMessage $ ExpandTowards L)
-- > , ((modm .|. altMask,               xK_j     ), sendMessage $ ExpandTowards D)
-- > , ((modm .|. altMask,               xK_k     ), sendMessage $ ExpandTowards U)
-- > , ((modm .|. altMask .|. ctrlMask , xK_l     ), sendMessage $ ShrinkFrom R)
-- > , ((modm .|. altMask .|. ctrlMask , xK_h     ), sendMessage $ ShrinkFrom L)
-- > , ((modm .|. altMask .|. ctrlMask , xK_j     ), sendMessage $ ShrinkFrom D)
-- > , ((modm .|. altMask .|. ctrlMask , xK_k     ), sendMessage $ ShrinkFrom U)
-- > , ((modm,                           xK_r     ), sendMessage Rotate)
-- > , ((modm,                           xK_s     ), sendMessage Swap)
--
-- Here's an alternative key mapping, this time using additionalKeysP,
-- arrow keys, and slightly different behavior when resizing windows
--
-- > , ("M-M1-<Left>",    sendMessage $ ExpandTowards L)
-- > , ("M-M1-<Right>",   sendMessage $ ShrinkFrom L)
-- > , ("M-M1-<Up>",      sendMessage $ ExpandTowards U)
-- > , ("M-M1-<Down>",    sendMessage $ ShrinkFrom U)
-- > , ("M-M1-C-<Left>",  sendMessage $ ShrinkFrom R)
-- > , ("M-M1-C-<Right>", sendMessage $ ExpandTowards R)
-- > , ("M-M1-C-<Up>",    sendMessage $ ShrinkFrom D)
-- > , ("M-M1-C-<Down>",  sendMessage $ ExpandTowards D)
-- > , ("M-s",            sendMessage $ BSP.Swap)
-- > , ("M-M1-s",         sendMessage $ Rotate) ]
--

-- |Message for flipping the tree (horizontal/vertical mirroring)
data TreeFlip = FlipH | FlipV deriving Typeable
instance Message TreeFlip

-- |Message for rotating the binary tree around the parent node of the window to the left or right
data TreeRotate = RotateL | RotateR deriving Typeable
instance Message TreeRotate

-- |Message to balance the tree - the current splits are forgotten and the layout is rebuilt
data TreeBalance = Balance deriving Typeable
instance Message TreeBalance

-- |Message to circulate the windows
data Circulate = CirculateL | CirculateR deriving Typeable
instance Message Circulate

-- |Message for rotating a split (horizontal/vertical) in the BSP
data Rotate = Rotate deriving Typeable
instance Message Rotate

-- |Message for resizing one of the cells in the BSP
data ResizeDirectional = ExpandTowards Direction2D | ShrinkFrom Direction2D | MoveSplit Direction2D deriving Typeable
instance Message ResizeDirectional

-- |Message for swapping the left child of a split with the right child of split
data Swap = Swap deriving Typeable
instance Message Swap

data Axis = Horizontal | Vertical deriving (Show, Read, Eq)

oppositeDirection :: Direction2D -> Direction2D
oppositeDirection U = D
oppositeDirection D = U
oppositeDirection L = R
oppositeDirection R = L

oppositeAxis :: Axis -> Axis
oppositeAxis Vertical = Horizontal
oppositeAxis Horizontal = Vertical

toAxis :: Direction2D -> Axis
toAxis U = Horizontal
toAxis D = Horizontal
toAxis L = Vertical
toAxis R = Vertical

split :: Axis -> Rational -> Rectangle -> (Rectangle, Rectangle)
split Horizontal r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw sh'
    r2 = Rectangle sx (sy + fromIntegral sh') sw (sh - sh')
    sh' = floor $ fromIntegral sh * r
split Vertical r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw' sh
    r2 = Rectangle (sx + fromIntegral sw') sy (sw - sw') sh
    sw' = floor $ fromIntegral sw * r

data Split = Split { axis :: Axis
                   , ratio :: Rational
                   } deriving (Show, Read, Eq)

oppositeSplit :: Split -> Split
oppositeSplit (Split d r) = Split (oppositeAxis d) r

increaseRatio :: Split -> Rational -> Split
increaseRatio (Split d r) delta = Split d (min 0.9 (max 0.1 (r + delta)))

resizeDiff :: Rational
resizeDiff = 0.05

data Tree a = Leaf Int | Node { value :: a
                          , left :: Tree a
                          , right :: Tree a
                          } deriving (Show, Read, Eq)

numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node _ l r) = numLeaves l + numLeaves r

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
goLeft (Leaf _, _) = Nothing
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Leaf _, _) = Nothing
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
goToNthLeaf _ z@(Leaf _, _) = Just z
goToNthLeaf n z@(t, _) =
  if numLeaves (left t) > n
  then do z' <- goLeft z
          goToNthLeaf n z'
  else do z' <- goRight z
          goToNthLeaf (n - (numLeaves . left $ t)) z'

splitCurrentLeaf :: Zipper Split -> Maybe (Zipper Split)
splitCurrentLeaf (Leaf _, []) = Just (Node (Split Vertical 0.5) (Leaf 0) (Leaf 0), [])
splitCurrentLeaf (Leaf _, crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) 0.5) (Leaf 0) (Leaf 0), crumb:cs)
splitCurrentLeaf _ = Nothing

removeCurrentLeaf :: Zipper a -> Maybe (Zipper a)
removeCurrentLeaf (Leaf _, []) = Nothing
removeCurrentLeaf (Leaf _, LeftCrumb _ r:cs) = Just (r, cs)
removeCurrentLeaf (Leaf _, RightCrumb _ l:cs) = Just (l, cs)
removeCurrentLeaf _ = Nothing

rotateCurrentLeaf :: Zipper Split -> Maybe (Zipper Split)
rotateCurrentLeaf (Leaf n, []) = Just (Leaf n, [])
rotateCurrentLeaf (Leaf n, c:cs) = Just (Leaf n, modifyParentVal oppositeSplit c:cs)
rotateCurrentLeaf _ = Nothing

swapCurrentLeaf :: Zipper a -> Maybe (Zipper a)
swapCurrentLeaf (Leaf n, []) = Just (Leaf n, [])
swapCurrentLeaf (Leaf n, c:cs) = Just (Leaf n, swapCrumb c:cs)
swapCurrentLeaf _ = Nothing

isAllTheWay :: Direction2D -> Zipper Split -> Bool
isAllTheWay _ (_, []) = True
isAllTheWay R (_, LeftCrumb s _:_)
  | axis s == Vertical = False
isAllTheWay L (_, RightCrumb s _:_)
  | axis s == Vertical = False
isAllTheWay D (_, LeftCrumb s _:_)
  | axis s == Horizontal = False
isAllTheWay U (_, RightCrumb s _:_)
  | axis s == Horizontal = False
isAllTheWay dir z = fromMaybe False $ goUp z >>= Just . isAllTheWay dir

expandTreeTowards :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
expandTreeTowards _ z@(_, []) = Just z
expandTreeTowards dir z
  | isAllTheWay dir z = shrinkTreeFrom (oppositeDirection dir) z
expandTreeTowards R (t, LeftCrumb s r:cs)
  | axis s == Vertical = Just (t, LeftCrumb (increaseRatio s resizeDiff) r:cs)
expandTreeTowards L (t, RightCrumb s l:cs)
  | axis s == Vertical = Just (t, RightCrumb (increaseRatio s (-resizeDiff)) l:cs)
expandTreeTowards D (t, LeftCrumb s r:cs)
  | axis s == Horizontal = Just (t, LeftCrumb (increaseRatio s resizeDiff) r:cs)
expandTreeTowards U (t, RightCrumb s l:cs)
  | axis s == Horizontal = Just (t, RightCrumb (increaseRatio s (-resizeDiff)) l:cs)
expandTreeTowards dir z = goUp z >>= expandTreeTowards dir

shrinkTreeFrom :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
shrinkTreeFrom _ z@(_, []) = Just z
shrinkTreeFrom R z@(_, LeftCrumb s _:_)
  | axis s == Vertical = Just z >>= goSibling >>= expandTreeTowards L
shrinkTreeFrom L z@(_, RightCrumb s _:_)
  | axis s == Vertical = Just z >>= goSibling >>= expandTreeTowards R
shrinkTreeFrom D z@(_, LeftCrumb s _:_)
  | axis s == Horizontal = Just z >>= goSibling >>= expandTreeTowards U
shrinkTreeFrom U z@(_, RightCrumb s _:_)
  | axis s == Horizontal = Just z >>= goSibling >>= expandTreeTowards D
shrinkTreeFrom dir z = goUp z >>= shrinkTreeFrom dir

-- Direction2D refers to which direction the divider should move.
autoSizeTree :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
autoSizeTree _ z@(_, []) = Just z
autoSizeTree d z =
    Just z >>= getSplit (toAxis d) >>= resizeTree d

-- resizing once found the correct split. YOU MUST FIND THE RIGHT SPLIT FIRST.
resizeTree :: Direction2D -> Zipper Split -> Maybe (Zipper Split)
resizeTree _ z@(_, []) = Just z
resizeTree R z@(_, LeftCrumb _ _:_) =
  Just z >>= expandTreeTowards R
resizeTree L z@(_, LeftCrumb _ _:_) =
  Just z >>= shrinkTreeFrom    R
resizeTree U z@(_, LeftCrumb _ _:_) =
  Just z >>= shrinkTreeFrom    D
resizeTree D z@(_, LeftCrumb _ _:_) =
  Just z >>= expandTreeTowards D
resizeTree R z@(_, RightCrumb _ _:_) =
  Just z >>= shrinkTreeFrom    L
resizeTree L z@(_, RightCrumb _ _:_) =
  Just z >>= expandTreeTowards L
resizeTree U z@(_, RightCrumb _ _:_) =
  Just z >>= expandTreeTowards U
resizeTree D z@(_, RightCrumb _ _:_) =
  Just z >>= shrinkTreeFrom    U

getSplit :: Axis -> Zipper Split -> Maybe (Zipper Split)
getSplit _ (_, []) = Nothing
getSplit d z =
 do let fs = findSplit d z
    if isNothing fs
      then findClosest d z
      else fs

findClosest :: Axis -> Zipper Split -> Maybe (Zipper Split)
findClosest _ z@(_, []) = Just z
findClosest d z@(_, LeftCrumb s _:_)
  | axis s == d = Just z
findClosest d z@(_, RightCrumb s _:_)
  | axis s == d = Just z
findClosest d z = goUp z >>= findClosest d

findSplit :: Axis -> Zipper Split -> Maybe (Zipper Split)
findSplit _ (_, []) = Nothing
findSplit d z@(_, LeftCrumb s _:_)
  | axis s == d = Just z
findSplit d z = goUp z >>= findSplit d

top :: Zipper a -> Zipper a
top z = case goUp z of
          Nothing -> z
          Just z' -> top z'

toTree :: Zipper a -> Tree a
toTree = fst . top

index :: W.Stack a -> Int
index s = case toIndex (Just s) of
            (_, Nothing) -> 0
            (_, Just int) -> int

data BinarySpacePartition a = BinarySpacePartition { balanceToggle :: Bool
                                                   , getTree :: Maybe (Tree Split) } deriving (Show, Read)

-- | an empty BinarySpacePartition to use as a default for adding windows to.
emptyBSP :: BinarySpacePartition a
emptyBSP = BinarySpacePartition False Nothing

makeBSP :: Tree Split -> BinarySpacePartition a
makeBSP = BinarySpacePartition False . Just

makeZipper :: BinarySpacePartition a -> Maybe (Zipper Split)
makeZipper (BinarySpacePartition _ Nothing) = Nothing
makeZipper (BinarySpacePartition _ (Just t)) = Just . toZipper $ t

size :: BinarySpacePartition a -> Int
size = maybe 0 numLeaves . getTree

zipperToBinarySpacePartition :: Maybe (Zipper Split) -> BinarySpacePartition b
zipperToBinarySpacePartition Nothing = emptyBSP
zipperToBinarySpacePartition (Just z) = BinarySpacePartition False . Just . toTree . top $ z

rectangles :: BinarySpacePartition a -> Rectangle -> [Rectangle]
rectangles (BinarySpacePartition _ Nothing) _ = []
rectangles (BinarySpacePartition _ (Just (Leaf _))) rootRect = [rootRect]
rectangles (BinarySpacePartition _ (Just node)) rootRect =
    rectangles (makeBSP . left $ node) leftBox ++
    rectangles (makeBSP . right $ node) rightBox
    where (leftBox, rightBox) = split (axis info) (ratio info) rootRect
          info = value node

doToNth :: (Zipper Split -> Maybe (Zipper Split)) -> BinarySpacePartition a -> Int -> BinarySpacePartition a
doToNth f b n = zipperToBinarySpacePartition $ makeZipper b >>= goToNthLeaf n >>= f

splitNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
splitNth (BinarySpacePartition _ Nothing) _ = makeBSP (Leaf 0)
splitNth b n = doToNth splitCurrentLeaf b n

removeNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
removeNth (BinarySpacePartition _ Nothing) _ = emptyBSP
removeNth (BinarySpacePartition _ (Just (Leaf _))) _ = emptyBSP
removeNth b n = doToNth removeCurrentLeaf b n

rotateNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
rotateNth (BinarySpacePartition _ Nothing) _ = emptyBSP
rotateNth b@(BinarySpacePartition _ (Just (Leaf _))) _ = b
rotateNth b n = doToNth rotateCurrentLeaf b n

swapNth :: BinarySpacePartition a -> Int -> BinarySpacePartition a
swapNth (BinarySpacePartition _ Nothing) _ = emptyBSP
swapNth b@(BinarySpacePartition _ (Just (Leaf _))) _ = b
swapNth b n = doToNth swapCurrentLeaf b n

growNthTowards :: Direction2D -> BinarySpacePartition a -> Int -> BinarySpacePartition a
growNthTowards _ (BinarySpacePartition _ Nothing) _ = emptyBSP
growNthTowards _ b@(BinarySpacePartition _ (Just (Leaf _))) _ = b
growNthTowards dir b n = doToNth (expandTreeTowards dir) b n

shrinkNthFrom :: Direction2D -> BinarySpacePartition a -> Int -> BinarySpacePartition a
shrinkNthFrom _ (BinarySpacePartition _ Nothing) _ = emptyBSP
shrinkNthFrom _ b@(BinarySpacePartition _ (Just (Leaf _))) _ = b
shrinkNthFrom dir b n = doToNth (shrinkTreeFrom dir) b n

autoSizeNth :: Direction2D -> BinarySpacePartition a -> Int -> BinarySpacePartition a
autoSizeNth _ (BinarySpacePartition _ Nothing) _ = emptyBSP
autoSizeNth _ b@(BinarySpacePartition _ (Just (Leaf _))) _ = b
autoSizeNth dir b n = doToNth (autoSizeTree dir) b n

-- rotate tree left or right around parent of nth leaf
rotateTreeNth :: Direction2D -> BinarySpacePartition a -> Int -> BinarySpacePartition a
rotateTreeNth _ (BinarySpacePartition _ Nothing) _ = emptyBSP
rotateTreeNth U b _ = b
rotateTreeNth D b _ = b
rotateTreeNth dir b@(BinarySpacePartition _ (Just t)) n =
  doToNth (\t -> case goUp t of
                Nothing     -> Just t
                Just (t, c) -> Just (rot dir t, c)) b n

-- right or left rotation of a (sub)tree, so effect if rotation not possible
rot dir (Leaf n) = (Leaf n)
rot R n@(Node _ (Leaf _) _) = n
rot L n@(Node _ _ (Leaf _)) = n
rot R (Node sp (Node sp2 l2 r2) r) = Node sp2 l2 (Node sp r2 r)
rot L (Node sp l (Node sp2 l2 r2)) = Node sp2 (Node sp l l2) r2

-- swap all siblings along one axis, also invert ratio for symmetry -> "mirror" layout
flipTree :: Axis -> BinarySpacePartition a -> BinarySpacePartition a
flipTree ax (BinarySpacePartition _ Nothing) = emptyBSP
flipTree ax (BinarySpacePartition bal (Just t)) = BinarySpacePartition bal $ Just $ f ax t
  where f ax (Leaf n) = Leaf n
        f ax (Node sp l r)
         | ax /= axis sp = Node sp{ratio=1 - ratio sp} (f ax r) (f ax l)
         | otherwise    = Node sp (f ax l) (f ax r)

-- tries to AVL-balance given tree (ignoring the balance flag here for reasons!)
balanceTree :: BinarySpacePartition a -> BinarySpacePartition a
balanceTree (BinarySpacePartition _ Nothing) = emptyBSP
balanceTree (BinarySpacePartition bal (Just t)) = BinarySpacePartition bal $ Just $ balance t
  where balance (Leaf n) = Leaf n
        balance n@(Node s l r)
         | heightDiff n == 2 && heightDiff l == -1 = balance $ Node s (rot L l) r
         | heightDiff n == -2 && heightDiff l == 1 = balance $ Node s l (rot R r)
         | heightDiff n == 2 && heightDiff l == 1 = rot R n
         | heightDiff n == -2 && heightDiff l == -1 = rot L n
         | otherwise = Node s (balance l) (balance r)
         where height (Leaf _) = 0
               height (Node _ l r) = 1 + max (height l) (height r)
               heightDiff (Leaf _) = 0
               heightDiff (Node _ l r) = height l - height r

-- attempt to rotate splits optimally in order choose more quad-like rects
optimizeOrientation :: Rectangle -> BinarySpacePartition a -> BinarySpacePartition a
optimizeOrientation r (BinarySpacePartition _ Nothing) = emptyBSP
optimizeOrientation r (BinarySpacePartition bal (Just t)) = BinarySpacePartition bal $ Just $ opt t r
  where opt (Leaf v) rect = (Leaf v)
        opt (Node sp l r) rect = Node sp' (opt l lrect) (opt r rrect)
         where (Rectangle _ _ w1 h1,Rectangle _ _ w2 h2) = split (axis sp) (ratio sp) rect
               (Rectangle _ _ w3 h3,Rectangle _ _ w4 h4) = split (axis $ oppositeSplit sp) (ratio sp) rect
               f w h = if w > h then w'/h' else h'/w' where (w',h') = (fromIntegral w, fromIntegral h)
               wratio = min (f w1 h1) (f w2 h2)
               wratio' = min (f w3 h3) (f w4 h4)
               sp' = if wratio<wratio' then sp else oppositeSplit sp
               (lrect, rrect) = split (axis sp') (ratio sp') rect

-- traverse and collect all leave numbers, left to right
flattenLeaves :: BinarySpacePartition a -> [Int]
flattenLeaves (BinarySpacePartition _ Nothing) = []
flattenLeaves (BinarySpacePartition _ (Just t)) = flatten t
 where flatten (Leaf n) = [n]
       flatten (Node _ l r) = flatten l++flatten r

-- we do this before an action to look afterwards which leaves moved where
numerateLeaves :: BinarySpacePartition a -> BinarySpacePartition a
numerateLeaves b@(BinarySpacePartition _ Nothing) = b
numerateLeaves b@(BinarySpacePartition bal (Just t)) = BinarySpacePartition bal . Just . snd $ numerate 0 t
  where numerate n (Leaf _) = (n+1, Leaf n)
        numerate n (Node s l r) = (n'', Node s nl nr)
          where (n', nl) = numerate n l
                (n'', nr) = numerate n' r

-- shift the numbers by an offset -> circulate windows
circulateLeaves :: Int -> BinarySpacePartition a -> BinarySpacePartition a
circulateLeaves _ b@(BinarySpacePartition _ Nothing) = b
circulateLeaves n b@(BinarySpacePartition bal (Just t)) = BinarySpacePartition bal . Just $ circ t
  where circ (Leaf m) = Leaf $ (m+n) `mod` size b
        circ (Node s l r) = Node s (circ l) (circ r)

--move windows to new positions according to tree transformations, keeping focus on originally focused window
--CAREFUL here! introduce a bug here and have fun debugging as your windows start to disappear or explode
adjustStack :: Maybe (W.Stack Window)  --original stack
            -> Maybe (W.Stack Window)  --stack without floating windows
            -> [Window]                --just floating windows of this WS
            -> Maybe (BinarySpacePartition Window) -- Tree with numbered leaves telling what to move where
            -> Maybe (W.Stack Window)  --resulting stack
adjustStack orig Nothing _ _ = orig   --no new stack -> no changes
adjustStack orig _ _ Nothing = orig   --empty tree   -> no changes
adjustStack orig s fw (Just b) =
 if length ls<length ws then orig     --less leaves than non-floating windows -> tree incomplete, no changes
 else fromIndex ws' fid'
 where ws' = (mapMaybe ((flip M.lookup) wsmap) ls)++fw
       fid' = fromMaybe 0 $ elemIndex focused ws'
       wsmap = M.fromList $ zip [0..] ws -- map: old index in list -> window
       ls = flattenLeaves b             -- get new index ordering from tree
       (ws,fid) = toIndex s
       focused = ws !! (fromMaybe 0 $ fid)

--replace the window stack of the managed workspace with our modified stack
replaceStack :: Maybe (W.Stack Window) -> X ()
replaceStack s = do
  st <- get
  let wset = windowset st
      cur  = W.current wset
      wsp  = W.workspace cur
  put st{windowset=wset{W.current=cur{W.workspace=wsp{W.stack=s}}}} --I heard lenses make this better...

-- debug str = spawn $ "echo \""++str++"\" >> /tmp/xdebug"

instance LayoutClass BinarySpacePartition Window where
  doLayout b r s = return (zip ws rs, Just b') where
    ws = W.integrate s
    layout bsp
      | l == count = bsp{balanceToggle=False} --remove flag after tree is complete
      | l > count = layout $ bal $ splitNth bsp n
      | otherwise = layout $ bal $ removeNth bsp n
      where count = size bsp
            bal   = if balanceToggle b then optimizeOrientation r . balanceTree else id

    l = length ws
    n = index s
    b' = layout b
    rs =  rectangles b' r

  handleMessage b_orig m =
    do ws <- (W.stack . W.workspace . W.current) <$> gets windowset -- windows on this WS (with floating)
       fs <- (M.keys . W.floating) <$> gets windowset               -- all floating windows
       let lws = maybe Nothing (unfloat fs) ws                            -- tiled windows on WS
           lfs = (maybe [] W.integrate ws) \\ (maybe [] W.integrate lws)  -- untiled windows on WS
           b'  = lws >>= handleMesg         -- transform tree (concerns only tiled windows)
           ws' = adjustStack ws lws lfs b' -- apply transformation to window stack, reintegrate floating
       replaceStack ws'
       return b'
    where handleMesg s = msum [fmap (`rotate` s)   (fromMessage m)
                              ,fmap (`resize` s)   (fromMessage m)
                              ,fmap (`swap` s)     (fromMessage m)
                              ,fmap (`rotateTr` s) (fromMessage m)
                              ,fmap flipTr         (fromMessage m)
                              ,fmap balanceTr      (fromMessage m)
                              ,fmap circTr         (fromMessage m)
                              ]
          -- ignore messages if current focus is on floating window
          unfloat fs s = if W.focus s `elem` fs
                         then Nothing
                         else Just (s { W.up = W.up s \\ fs
                                      , W.down = W.down s \\ fs })

          b = numerateLeaves b_orig

          rotate Rotate s = rotateNth b $ index s
          swap Swap s = swapNth b $ index s
          resize (ExpandTowards dir) s = growNthTowards dir b $ index s
          resize (ShrinkFrom dir) s = shrinkNthFrom dir b $ index s
          resize (MoveSplit dir) s = autoSizeNth dir b $ index s
          rotateTr RotateL s = rotateTreeNth L b $ index s
          rotateTr RotateR s = rotateTreeNth R b $ index s

          flipTr FlipH = flipTree Horizontal b
          flipTr FlipV = flipTree Vertical b
          circTr CirculateL = circulateLeaves 1 b
          circTr CirculateR = circulateLeaves (-1) b
          balanceTr Balance = emptyBSP{balanceToggle=True}

  description _  = "BSP"

