{-# LANGUAGE TemplateHaskell #-}

module VHex.State.Buffer
( BufferContext
, toBufferContext, fromBufferContext
, bcSelected
, curHori
) where

import Data.Word (Word8)

import Lens.Micro ((&), (^.), (%~), (.~))

import Brick.Types
    ( Direction
    , EventM
    , Extent(..)
    , suffixLenses
    )
import Brick.Main (lookupExtent)
import Brick.Util (clamp)

import VHex.Types
    ( WindowState, wsBufferL, wsLayoutL, wsScrollPosL
    , Name(..)
    , Layout
    )
import VHex.Util (floorN, hexLength, fromDir)
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import qualified VHex.ByteView as BV
import qualified VHex.ListZipper as LZ

data BufferContext = BufferContext
    { bcBuf :: ByteZipper
    -- ^ Editing buffer contents and cursor position.
    , bcScroll :: Int
    -- ^ Rows of bytes visible.
    , bcRows :: Int
    -- ^ Number of columns per row.
    , bcCols :: Int
    }
suffixLenses ''BufferContext

-- TODO place these in reader
scrollOff = 5
bytesPerRowMultiple = 4

bytesPerRow :: Layout -> Int -> Int -> Int
bytesPerRow l w byteCount = max 1 $ floorN bytesPerRowMultiple maxBytes where
    offsetWidth = hexLength $ byteCount - 1
    linWidth = sum
             $ map ((+) <$> BV.displayWidth <*> BV.spaceWidth)
             $ LZ.toList l
    padding = LZ.length l - 1
    maxBytes = div (w - offsetWidth - padding) linWidth

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
                         , bcScroll = ws^.wsScrollPosL
                         , bcRows = rows
                         , bcCols = cols
                         }

fromBufferContext :: BufferContext -> WindowState -> WindowState
fromBufferContext bc = (wsBufferL    .~ bc^.bcBufL)
                     . (wsScrollPosL .~ bc^.bcScrollL)

bcCursor :: BufferContext -> Int
bcCursor = BZ.location . (^.bcBufL)

bcMoveTo :: Int -> BufferContext -> BufferContext
bcMoveTo pos = bcBufL %~ BZ.moveTo pos

bcSelected :: BufferContext -> Maybe Word8
bcSelected = BZ.selected . (^.bcBufL)

bcSize :: BufferContext -> Int
bcSize = BZ.length . (^.bcBufL)

followCursor :: BufferContext -> BufferContext
followCursor bc = bc & bcMoveTo newPos where
    bottomMargin = bcSize bc-1 - (bc^.bcRowsL)*(bc^.bcColsL-1)
    upperMargin = bcCursor bc + (bc^.bcRowsL)*(scrollOff+1-(bc^.bcColsL))
    minPos = clamp 0 upperMargin bottomMargin
    lowerMargin = bcCursor bc - (bc^.bcRowsL)*scrollOff
    newPos' = clamp minPos lowerMargin (bc^.bcScrollL)
    newPos = floorN (bc^.bcRowsL) newPos'

curHori :: Direction -> BufferContext -> BufferContext
curHori dir bc = bc & bcMoveTo newPos & followCursor
    where newPos = clamp 0 (bcSize bc) (bcCursor bc + fromDir dir)
