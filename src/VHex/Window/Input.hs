{-# LANGUAGE TemplateHaskell #-}

module VHex.Window.Input
( InputContext(..)
, InputM
, Input(..), fromInput, toInput

-- * Mode change operations
, enterInputMode
, exitInputMode

-- * Operations on input in context
, curHori, curVert
) where

import Data.Maybe (isJust)

import Control.Monad.Reader
import Control.Category ((>>>))

import Lens.Micro
import Brick.Types

import VHex.Types
import qualified VHex.ListZipper as LZ

import VHex.Window.Buffer
    ( Buffer, BufferM
    , fromBuffer, toBuffer
    , bCursor, bSelected, bSize, bReplace
    )
import qualified VHex.Window.Buffer as Buf
import VHex.Window.ByteView (ByteView)
import qualified VHex.Window.ByteView as BV

data InputContext = InputContext
    { icByteView :: ByteView
    , icInsMode :: InsMode
    }

type InputM = ReaderT InputContext BufferM

data Input = Input
    { iIs :: InputState
    , iBuf :: Buffer
    }
suffixLenses ''Input

toInput :: InputState -> WindowState -> Input
toInput is ws = Input { iIs = is
                      , iBuf = toBuffer ws
                      }

fromInput :: InsMode -> EditorState -> Input -> EditorState
fromInput im es i = es & esModeL .~ InputMode im (iIs i)
                       & esWindowL %~ fromBuffer (iBuf i)

exitInputMode :: Input -> InputM Input
exitInputMode = inputSave -- TODO remove if empty

enterInputMode :: Input -> InputM Input
enterInputMode = inputLoad

-- | Load selected word from buffer to string input.
inputLoad :: Input -> InputM Input
inputLoad i = do
    bv <- asks icByteView
    let newInput = case bSelected (i^.iBufL) of
                    Nothing -> LZ.empty
                    Just w -> LZ.fromList (BV.fromWord bv w)
    i & iIsL.isInputL .~ newInput & return

-- | Save string input to selected word in buffer.
inputSave :: Input -> InputM Input
inputSave i = do
    bv <- asks icByteView
    case i^.iIsL.isInputL&LZ.toList&BV.toWord bv of
        Nothing -> i & return
        Just w -> i & iBufL %~ bReplace w & return

-- | Apply monadic buffer operation on buffer within Input.
liftBuf :: (Buffer -> BufferM Buffer) -> Input -> InputM Input
liftBuf bufOp i = do
    newBuf <- lift $ bufOp $ iBuf i
    return i { iBuf = newBuf }

curVert :: Direction -> Input -> InputM Input
curVert dir = inputSave
               >=> liftBuf (Buf.curVert dir)
               >=> inputLoad

curHori :: Direction -> Input -> InputM Input
curHori dir i = do
    bv <- asks icByteView
    im <- asks icInsMode
    let dw = BV.displayWidth bv
        bufLen = i^.iBufL&bSize
        maxPos = case im of
            InsertMode -> bufLen
            ReplaceMode -> bufLen-1
        pos = i^.iIsL.isInputL&LZ.position
        onLeftEdge  = dir == Up   && pos == 0
        onRightEdge = dir == Down && pos == dw-1
        cursor = i^.iBufL&bCursor
        inputValid = isJust $ BV.toWord bv (i^.iIsL.isInputL&LZ.toList)
        nextEdge = case dir of Up -> LZ.end; Down -> LZ.beginning
        op
            | onLeftEdge  && cursor == 0      = return
            | onRightEdge && cursor == maxPos = return
            | inputValid && (onLeftEdge || onRightEdge) =
                inputSave >=> liftBuf (Buf.curHori dir)
                          >=> (iIsL.isInputL %~ nextEdge) >>> return
                          >=> inputLoad
            | onLeftEdge || onRightEdge = return
            | otherwise = iIsL.isInputL %~ LZ.move dir
                      >>> iIsL.isNewByteL .~ False
                      >>> inputSave
    op i
