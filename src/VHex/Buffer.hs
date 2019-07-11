{-# LANGUAGE TemplateHaskell #-}

module VHex.Buffer
( BufferM(..)
, BufferContext(..)
, Buffer(..), toBuffer, fromBuffer
, bSelected
, curHori
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

bCursor :: Buffer -> Int
bCursor = BZ.location . bBuf

bMove :: Int -> Buffer -> Buffer
bMove d = bBufL %~ BZ.move d

bMoveTo :: Int -> Buffer -> Buffer
bMoveTo pos = bBufL %~ BZ.moveTo pos

bSelected :: Buffer -> Maybe Word8
bSelected = BZ.selected . bBuf

bSize :: Buffer -> Int
bSize = BZ.length . bBuf

followCursor :: Buffer -> BufferM Buffer
followCursor b = do
    rows <- view bcRowsL
    cols <- view bcColsL
    scrollOff <- view $ bcConfigL.cfgScrollOffL

    let bottomMargin = bSize b-1 - cols*(rows-1)
        upperMargin = bCursor b + cols*(scrollOff+1-rows)
        minPos = clamp 0 upperMargin bottomMargin
        lowerMargin = bCursor b - cols*scrollOff
        newPos = clamp minPos lowerMargin (b^.bScrollL)
    return $ b & bScrollL .~ (floorN cols newPos)

curHori :: Direction -> Buffer -> BufferM Buffer
curHori dir b = b & bMove (fromDir dir) & followCursor
