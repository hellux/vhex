module VHex.Attributes
( attrCurrent
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
, attributes
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

attrCurrent :: AttrName
attrDef :: AttrName
attrCursorLine :: AttrName
attrSelected :: AttrName
attrSelectedFocused :: AttrName
attrInvalid :: AttrName
attrOffset :: AttrName
attrOffsetCursorLine :: AttrName
attrError :: AttrName
attrStatusLine :: AttrName
attrMode :: AttrName

attrCurrent = attrName "current"
attrDef = attrName "def"
attrCursorLine = attrName "cursorLine"
attrSelected = attrName "selected"
attrSelectedFocused = attrName "selectedFocused"
attrInvalid = attrName "invalid"
attrOffset = attrName "offset"
attrOffsetCursorLine = attrName "offsetCursorLine"
attrStatusLine = attrName "statusline"
attrMode = attrName "mode"
attrError = attrName "error"

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
