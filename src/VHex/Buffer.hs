{-# LANGUAGE TemplateHaskell #-}

module VHex.Buffer
( BufferContext, Buffer
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

type BufferM = Reader BufferContext

data Buffer = Buffer
    { bBuf :: ByteZipper
    -- ^ Editing buffer contents and cursor position.
    , bScroll :: Int
    -- ^ Rows of bytes visible.
    }
suffixLenses ''Buffer

{-
toBufferContext :: WindowState -> EventM Name BufferContext
toBufferContext ws = do
    extent <- lookupExtent EditorWindow
    let (width, height) = case extent of
                            Nothing -> (0, 0)
                            Just (Extent _ _ dims _) -> dims
    let rows = bytesPerRow (ws^.wsLayoutL)
                           width
                           (BZ.length (ws^.wsBufferL))
        cols = height
    return BufferContext { bcBuf = ws^.wsBufferL
                         , bScroll = ws^.wsScrollPosL
                         , bcRows = rows
                         , bcCols = cols
                         }

fromBufferContext :: BufferContext -> WindowState -> WindowState
fromBufferContext bc = (wsBufferL    .~ bc^.bBufL)
                     . (wsScrollPosL .~ bc^.bScrollL)
                         -}

bCursor :: Buffer -> Int
bCursor = BZ.location . (^.bBufL)

bMove :: Int -> Buffer -> Buffer
bMove d = bBufL %~ BZ.move d

bMoveTo :: Int -> Buffer -> Buffer
bMoveTo pos = bBufL %~ BZ.moveTo pos

bSelected :: Buffer -> Maybe Word8
bSelected = BZ.selected . (^.bBufL)

bSize :: Buffer -> Int
bSize = BZ.length . (^.bBufL)

followCursor :: Buffer -> BufferM Buffer
followCursor b = do
    rows <- view bcRowsL
    cols <- view bcColsL
    scrollOff <- view $ bcConfigL.cfgScrollOffL

    let bottomMargin = bSize b-1 - rows*(cols-1)
        upperMargin = bCursor b + rows*(scrollOff+1-cols)
        minPos = clamp 0 upperMargin bottomMargin
        lowerMargin = bCursor b - rows*scrollOff
        newPos = clamp minPos lowerMargin (b^.bScrollL)

    return $ b & bMoveTo (floorN rows newPos)

curHori :: Direction -> Buffer -> BufferM Buffer
curHori dir b = b & bMove (fromDir dir) & return
