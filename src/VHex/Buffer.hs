{-# LANGUAGE TemplateHaskell #-}

module VHex.Buffer
( BufferM(..)
, BufferContext(..)
, Buffer(..), toBuffer, fromBuffer

, cursor
, selected
, size
, replace

, curHori, curVert
) where

import Data.Word (Word8)

import Control.Monad.Reader

import Lens.Micro ((&), (^.), (%~), (.~))
import Lens.Micro.Mtl (view)

import Brick.Types
    ( Direction
    , EventM
    , Extent(..)
    , suffixLenses
    )
import Brick.Main (lookupExtent)
import Brick.Util (clamp)

import VHex.Types
import VHex.Util (floorN, hexLength, fromDir)
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import qualified VHex.ByteView as BV
import qualified VHex.ListZipper as LZ

data BufferContext = BufferContext
    { bcConfig :: VHexConfig
    -- ^ User options.
    , bcRows :: Int
    -- ^ Number of columns per row.
    , bcCols :: Int
    -- ^ Number
    }
suffixLenses ''BufferContext

data Buffer = Buffer
    { bBuf :: ByteZipper
    -- ^ Editing buffer contents and cursor position.
    , bScroll :: Int
    -- ^ Rows of bytes visible.
    }
suffixLenses ''Buffer

type BufferM = Reader BufferContext

toBuffer :: WindowState -> Buffer
toBuffer ws = Buffer { bBuf = wsBuffer ws
                     , bScroll = wsScrollPos ws
                     }

fromBuffer :: Buffer -> WindowState -> WindowState
fromBuffer buf ws = ws { wsBuffer = bBuf buf
                       , wsScrollPos = bScroll buf
                       }

cursor :: Buffer -> Int
cursor = BZ.location . bBuf

move :: Int -> Buffer -> Buffer
move d = bBufL %~ BZ.move d

moveTo :: Int -> Buffer -> Buffer
moveTo pos = bBufL %~ BZ.moveTo pos

selected :: Buffer -> Maybe Word8
selected = BZ.selected . bBuf

replace :: Word8 -> Buffer -> Buffer
replace w = bBufL %~ BZ.replace w

size :: Buffer -> Int
size = BZ.length . bBuf

followCursor :: Buffer -> BufferM Buffer
followCursor b = do
    rows <- asks bcRows
    cols <- asks bcCols
    scrollOff <- view $ bcConfigL.cfgScrollOffL

    let bottomMargin = size b-1 - cols*(rows-1)
        upperMargin = cursor b + cols*(scrollOff+1-rows)
        minPos = clamp 0 upperMargin bottomMargin
        lowerMargin = cursor b - cols*scrollOff
        newPos = clamp minPos lowerMargin (b^.bScrollL)
    b & bScrollL .~ floorN cols newPos & return

curHori :: Direction -> Buffer -> BufferM Buffer
curHori dir b = b & move (fromDir dir) & followCursor

curVert :: Direction -> Buffer -> BufferM Buffer
curVert dir b = do
    step <- asks bcCols
    let d = fromDir dir
        newPos = clamp 0 (size b-1) (cursor b + d*step)
    b & moveTo newPos & followCursor
