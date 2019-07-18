{-|
Module      : VHex.Main
Description : Global widget attributes.
Copyright   : (c) Noah Hellman, 2019
License     : GPL-3
Maintainer  : noah.hellman@protonmail.com
Stability   : unstable
Portability : not portable
-}

module VHex.Attributes
( attributes
-- * Attribute names
, attrCurrent
, attrDef
, attrCursorLine
, attrSelected
, attrSelectedFocused
, attrInvalid
, attrOffset
, attrOffsetCursorLine
, attrError
, attrStatusLine
, attrMode
) where

import Graphics.Vty
    ( withStyle, currentAttr
    , underline, bold
    , brightWhite, brightRed, brightBlack, brightYellow
    , black
    , rgbColor
    )
import Brick.Util (on, fg, bg)
import Brick.AttrMap (attrName, AttrName, attrMap, AttrMap)

-- | Keep the current attribute unchanged.
attrCurrent :: AttrName
attrCurrent = attrName "current"

-- | Default attribute, foreground color on background color.
attrDef :: AttrName
attrDef = attrName "def"

-- | Attribute of selected line.
attrCursorLine :: AttrName
attrCursorLine = attrName "cursorLine"

-- | Attribute of selected byte in an unfocused frame.
attrSelected :: AttrName
attrSelected = attrName "selected"

-- | Attribute of selected byte in focused frame.
attrSelectedFocused :: AttrName
attrSelectedFocused = attrName "selectedFocused"

-- | Attribute of invalid input.
attrInvalid :: AttrName
attrInvalid = attrName "invalid"

-- | Attribute of offset numbers / addresses.
attrOffset :: AttrName
attrOffset = attrName "offset"

-- | Attribute of offset for selected line.
attrOffsetCursorLine = attrName "offsetCursorLine"
attrOffsetCursorLine :: AttrName

-- | Attribute of the status line.
attrStatusLine = attrName "statusline"
attrStatusLine :: AttrName

-- | Attribute of the mode indicator text on the command line.
attrMode :: AttrName
attrMode = attrName "mode"

-- | Attribute of error messages on the command line.
attrError :: AttrName
attrError = attrName "error"

-- | The attribute map binding attribute names to actual attributes.
attributes :: AttrMap
attributes = attrMap mempty
    [ (attrCurrent,             currentAttr)
    , (attrDef,                 fg foreground)
    , (attrCursorLine,          bg grey23)
    , (attrSelected,            bg grey23 `withStyle` underline)
    , (attrSelectedFocused,     background `on` foreground)
    , (attrInvalid,             brightRed `on` grey23)
    , (attrOffset,              fg brightBlack)
    , (attrOffsetCursorLine,    brightYellow `on` grey23)
    , (attrStatusLine,          bg grey30)
    , (attrMode,                fg brightYellow `withStyle` bold)
    , (attrError,               grey15 `on` brightRed `withStyle` bold)
    ] where foreground = brightWhite
            background = black
            grey15 = rgbColor (38::Int) (38::Int) (38::Int)
            grey23 = rgbColor (58::Int) (58::Int) (58::Int)
            grey30 = rgbColor (78::Int) (78::Int) (78::Int)
