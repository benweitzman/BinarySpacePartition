BinarySpacePartition
====================

#### NOTICE: This layout is in the XMonadContrib darcs (though not the latest version as of 11/12/14)
So unless you are making changes or need the latest version I suggest using that. Feel free to make changes
here and or directly to xmonad contrib through darcs, I will pull down any upstream changes periodically. 

====================

BinarySpacePartition (BSP) is an XMonad Layout where new windows will split the focused window in half. 
This is based off of https://github.com/baskerville/bspwm.

The BSP can be manipulated to create highly configurable layouts. By default new windows split the
current window by alternating vertical and horizontal splits. Nodes of the tree can be rotated to change the
axis of the split, and the two children of a node can be swapped. 

The layout provides the following messages:

* `Rotate` which rotates a split between vertical and horizontal
* `Swap` which swaps to sibling nodes
* `ExpandTowards dir` which takes a Direction2D argument (`U`, `D`, `L`, `R`) and expands the selected window's border
in that direction
* `ShrinkFrom dir` which takes a Direction2D argument and shrinks the selected window's border from that direction
* `MoveSplit dir` which takes a Direction2D argument and tries to intelligently move some border in that direction. 
This mode seems a bitmore intuitive to some people.

To use BSP, import the module in your `~/.xmonad/xmonad.hs` file:

```
import XMonad.Layout.BinarySpacePartition
```

Then add the layout, using the default BSP:

``` 
myLayout = emptyBSP ||| etc ..
```

It will be helpful to add the following keybindings:

```
, ((modm .|. altMask,               xK_l     ), sendMessage $ ExpandTowards R)
, ((modm .|. altMask,               xK_h     ), sendMessage $ ExpandTowards L)
, ((modm .|. altMask,               xK_j     ), sendMessage $ ExpandTowards D)
, ((modm .|. altMask,               xK_k     ), sendMessage $ ExpandTowards U)
, ((modm .|. altMask .|. ctrlMask , xK_l     ), sendMessage $ ShrinkFrom R)
, ((modm .|. altMask .|. ctrlMask , xK_h     ), sendMessage $ ShrinkFrom L)
, ((modm .|. altMask .|. ctrlMask , xK_j     ), sendMessage $ ShrinkFrom D)
, ((modm .|. altMask .|. ctrlMask , xK_k     ), sendMessage $ ShrinkFrom U)
, ((modm,                           xK_r     ), sendMessage Rotate)
, ((modm,                           xK_s     ), sendMessage Swap)
```

Here's an alternative key mapping, this time using additionalKeysP,
arrow keys, and slightly different behavior when resizing windows

```
, ("M-M1-<Left>",    sendMessage $ ExpandTowards L)
, ("M-M1-<Right>",   sendMessage $ ShrinkFrom L)
, ("M-M1-<Up>",      sendMessage $ ExpandTowards U)
, ("M-M1-<Down>",    sendMessage $ ShrinkFrom U)
, ("M-M1-C-<Left>",  sendMessage $ ShrinkFrom R)
, ("M-M1-C-<Right>", sendMessage $ ExpandTowards R)
, ("M-M1-C-<Up>",    sendMessage $ ShrinkFrom D)
, ("M-M1-C-<Down>",  sendMessage $ ExpandTowards D)
, ("M-s",            sendMessage $ Swap)
, ("M-M1-s",         sendMessage $ Rotate)
```

And to use the alternate resizing mode:

```
, ((myModKey .|. controlMask, xK_Left   ), sendMessage $ MoveSplit L)
```
