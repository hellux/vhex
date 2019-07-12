{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module VHex.Input
( InputContext(..)
, InputM
, Input(..), fromInput, toInput

, inputLoad
, inputCurHori
) where

import Data.Word (Word8)
import Data.Maybe (isJust)

import Control.Monad.Reader
import Control.Category ((>>>))

import Lens.Micro
import Lens.Micro.Mtl
import Brick.Types

import qualified VHex.Buffer as Buf
import VHex.Types
import VHex.ByteView (ByteView)
import qualified VHex.ByteView as BV
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

data InputContext = InputContext
    { icByteView :: ByteView
    , icInsMode :: InsMode
    }
suffixLenses ''InputContext

type InputM = ReaderT InputContext Buf.BufferM

data Input = Input
    { iIs :: InputState 
    , iBuf :: Buf.Buffer
    }
suffixLenses ''Input

toInput :: InputState -> WindowState -> Input
toInput is ws = Input { iIs = is
                      , iBuf = Buf.toBuffer ws
                      }

fromInput :: Input -> EditorState -> EditorState
fromInput i es = es & esModeL %~ mode
                    & esWindowL %~ Buf.fromBuffer (iBuf i)
    where mode (InputMode im _) = InputMode im (iIs i)
          mode m = m

inputLoad :: Input -> InputM Input
inputLoad i = do
    bv <- asks icByteView
    let newInput = case Buf.selected (i^.iBufL) of
                    Nothing -> LZ.empty
                    Just w -> LZ.fromList (BV.fromWord bv w)
    i & iIsL.isInputL .~ newInput & return

inputSave :: Input -> InputM Input
inputSave = return

curHori :: Direction -> Input -> InputM Input
curHori dir i = do
    newBuf <- lift $ Buf.curHori dir $ iBuf i
    return i { iBuf = newBuf }

inputCurHori :: Direction -> Input -> InputM Input
inputCurHori dir i = do
    bv <- asks icByteView
    im <- asks icInsMode
    let dw = BV.displayWidth bv
        bufLen = i^.iBufL&Buf.size
        maxPos = case im of
            InsertMode -> bufLen
            ReplaceMode -> bufLen-1
        pos = i^.iIsL.isInputL&LZ.position
        onLeftEdge  = dir == Up   && pos == 0
        onRightEdge = dir == Down && pos == dw-1
        cursor = i^.iBufL&Buf.cursor
        inputValid = isJust $ BV.toWord bv (i^.iIsL.isInputL&LZ.toList)
        nextEdge = case dir of Up -> LZ.end; Down -> LZ.beginning
    case () of
     _
        | onLeftEdge  && cursor == 0      -> i & return
        | onRightEdge && cursor == maxPos -> i & return
        | inputValid && (onLeftEdge || onRightEdge) ->
              i & inputSave
              >>= curHori dir
              >>= ((iIsL.isInputL %~ nextEdge) >>> return)
              >>= inputLoad
        | onLeftEdge || onRightEdge -> i & return
        | otherwise -> i & iIsL.isInputL %~ LZ.move dir
                         & iIsL.isNewByteL .~ False
                         & inputSave
