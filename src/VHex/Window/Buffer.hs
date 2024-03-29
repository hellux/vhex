{-|
Module      : VHex.Main
Description : Operations on buffer.
Copyright   : (c) Noah Hellman, 2019
License     : GPL-3
Maintainer  : noah.hellman@protonmail.com
Stability   : unstable
Portability : not portable
-}

module VHex.Window.Buffer
( BufferM
, BufferContext(..)

-- * Buffer
, Buffer(..), toBuffer, fromBuffer

-- * ByteZipper wrappers
, bCursor
, bSelected
, bSize
, bInsert, bReplace
, bRemove
, bMove

-- * Operations on buffer in context.

-- ** Cursor movement
, curHori, curVert
, curBeginning, curEnd
, curTop, curBottom

-- ** Scrolling
, scroll
, scrollHalfPage

-- ** Modification
, removeWord
, removeWordPrev
) where

import Data.Word (Word8)

import Control.Monad.Reader
import Control.Category ((>>>))

import Lens.Micro ((&))

import Brick.Types (Direction(..))

import VHex.Types
import VHex.Util (floorN, fromDir, clamp)
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ

data BufferContext = BufferContext
    { bcConfig :: VHexConfig
    -- ^ User options.
    , bcRows :: Int
    -- ^ Number of columns per row.
    , bcCols :: Int
    -- ^ Number
    }

data Buffer = Buffer
    { bBuf :: ByteZipper
    -- ^ Editing buffer contents and cursor position.
    , bScroll :: Int
    -- ^ Rows of bytes visible.
    }

type BufferM = Reader BufferContext

toBuffer :: WindowState -> Buffer
toBuffer ws = Buffer { bBuf = wsBuffer ws
                     , bScroll = wsScrollPos ws
                     }

fromBuffer :: Buffer -> WindowState -> WindowState
fromBuffer buf ws = ws { wsBuffer = bBuf buf
                       , wsScrollPos = bScroll buf
                       }

-- Wrappers for ByteZipper operations on contained buffer.

bCursor :: Buffer -> Int
bCursor = BZ.location . bBuf

bMove :: Int -> Buffer -> Buffer
bMove d b = b { bBuf = BZ.move d $ bBuf b }

bMoveTo :: Int -> Buffer -> Buffer
bMoveTo pos b = b { bBuf = BZ.moveTo pos $ bBuf b }

bSelected :: Buffer -> Maybe Word8
bSelected = BZ.selected . bBuf

bInsert :: Word8 -> Buffer -> Buffer
bInsert w b = b { bBuf = BZ.insert w $ bBuf b }

bReplace :: Word8 -> Buffer -> Buffer
bReplace w b = b { bBuf = BZ.replace w $ bBuf b }

bRemove :: Buffer -> Buffer
bRemove b = b { bBuf = BZ.remove $ bBuf b }

bSize :: Buffer -> Int
bSize = BZ.length . bBuf

-- Operations on buffers in context.

-- | Keep cursor within bounds of the buffer.
containCursor :: Buffer -> BufferM Buffer
containCursor b = let clamped = clamp 0 (bSize b-1) (bCursor b)
                  in b & bMoveTo clamped & return

-- | Move cursor to keep it in view when scrolling.
keepCursor :: Buffer -> BufferM Buffer
keepCursor b = do
    cols <- asks bcCols
    rows <- asks bcRows
    scrollOff <- cfgScrollOff <$> asks bcConfig

    let curRow = div (bCursor b - bScroll b) cols
        newRow = clamp scrollOff (rows-scrollOff-1) curRow
        newPos = bCursor b + ((newRow-curRow)*cols)
        finalPos = min (bSize b-1) newPos
    b & bMoveTo finalPos & return

-- | Scroll to keep cursor in view when moving.
followCursor :: Buffer -> BufferM Buffer
followCursor b = do
    rows <- asks bcRows
    cols <- asks bcCols
    scrollOff <- cfgScrollOff <$> asks bcConfig

    let bottomMargin = bSize b-1 - cols*(rows-1)
        upperMargin = bCursor b + cols*(scrollOff+1-rows)
        minPos = clamp 0 upperMargin bottomMargin
        lowerMargin = bCursor b - cols*scrollOff
        newPos = clamp minPos lowerMargin (bScroll b)
    b { bScroll = floorN cols newPos } & return

curHori :: Direction -> Buffer -> BufferM Buffer
curHori dir b = b & bMove (fromDir dir) & containCursor >>= followCursor

curVert :: Direction -> Buffer -> BufferM Buffer
curVert dir b = do
    step <- asks bcCols
    b & bMoveTo (bCursor b + fromDir dir*step) & containCursor >>= followCursor

curBeginning :: Buffer -> BufferM Buffer
curBeginning b = do
    cols <- asks bcCols
    let newPos = floorN cols (bCursor b)
    b & bMoveTo newPos & return

curEnd :: Buffer -> BufferM Buffer
curEnd b = do
    cols <- asks bcCols
    let lineEnd = floorN cols (bCursor b) + cols - 1
        newPos = min lineEnd (bSize b - 1)
    b & bMoveTo newPos & return

curTop :: Buffer -> BufferM Buffer
curTop = bMoveTo 0 >>> followCursor

curBottom :: Buffer -> BufferM Buffer
curBottom b = b & bMoveTo (bSize b-1) & followCursor

scroll :: Direction -> Buffer -> BufferM Buffer
scroll dir b = do
    cols <- asks bcCols
    let prev = bScroll b
        maxPos = floorN cols (bSize b-1)
    b { bScroll = clamp 0 maxPos (prev+fromDir dir*cols) } & keepCursor

scrollHalfPage :: Direction -> Buffer -> BufferM Buffer
scrollHalfPage dir b = do
    cols <- asks bcCols
    rows <- asks bcRows
    let diff = fromDir dir * (div rows 2 * cols)
        newPos = clamp 0 (bSize b-1) (bCursor b+diff)
        newScroll = let maxScroll = floorN cols (bSize b-1)
                    in clamp 0 maxScroll (bScroll b + diff)
    b { bScroll = newScroll } & bMoveTo newPos & followCursor

removeWord :: Buffer -> BufferM Buffer
removeWord  = bRemove >>> containCursor

removeWordPrev :: Buffer -> BufferM Buffer
removeWordPrev b = if bCursor b == 0
                    then b & return
                    else b & (curHori Up >=> bRemove >>> followCursor)
