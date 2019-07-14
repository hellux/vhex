{-# LANGUAGE TemplateHaskell #-}

module VHex.Window.Input
( InputContext(..)
, InputM
, Input(..), fromInput, toInput

-- * Mode change operations
, enterInputMode
, enterInputModeAppend
, exitInputMode

-- * Operations on input in context
, curWord, curHori, curVert
, insert, replace
, remove, delete
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
    , bCursor, bSelected, bSize, bReplace, bInsert
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

enterInputMode :: Input -> InputM Input
enterInputMode = load

enterInputModeAppend :: Input -> InputM Input
enterInputModeAppend = curWord Down >=> load

exitInputMode :: Input -> InputM Input
exitInputMode = leave >=> curWord Up

-- | Apply monadic buffer operation on buffer within Input.
liftBuf :: (Buffer -> BufferM Buffer) -> Input -> InputM Input
liftBuf bufOp i = do
    newBuf <- lift $ bufOp $ iBuf i
    return i { iBuf = newBuf }

-- | Load selected word from buffer to string input.
load :: Input -> InputM Input
load i = do
    bv <- asks icByteView
    let newInput = case bSelected (i^.iBufL) of
                    Nothing -> LZ.empty
                    Just w -> LZ.fromList (BV.fromWord bv w)
    i & iIsL.isInputL .~ newInput & return

-- | Save string input to selected word in buffer.
save :: Input -> InputM Input
save i = do
    bv <- asks icByteView
    case i^.iIsL.isInputL&LZ.toList&BV.toWord bv of
        Nothing -> i & return
        Just w -> i & iBufL %~ bReplace w & return

leave :: Input -> InputM Input
leave i = do
    mode <- asks icInsMode
    if mode == InsertMode && (i^.iIsL.isInputL & LZ.null)
        then i & liftBuf Buf.removeWord
        else i & save

valid :: Input -> InputM Bool
valid i = do
    bv <- asks icByteView
    return $ isJust $ BV.toWord bv (i^.iIsL.isInputL&LZ.toList)

curWord :: Direction -> Input -> InputM Input
curWord dir = save
          >=> liftBuf (Buf.curHori dir)
          >=> (iIsL.isNewByteL .~ True) >>> return
          >=> load

curVert :: Direction -> Input -> InputM Input
curVert dir i = do
    inputValid <- valid i
    if inputValid
        then i & save
             >>= liftBuf (Buf.curVert dir)
             >>= (iIsL.isNewByteL .~ True >>> return)
             >>= load
             >>= (iIsL.isInputL %~ LZ.moveTo pos >>> return)
        else i & return
    where pos = i^.iIsL.isInputL & LZ.position

curHori :: Direction -> Input -> InputM Input
curHori dir i = do
    bv <- asks icByteView
    im <- asks icInsMode
    inputValid <- valid i
    let dw = BV.displayWidth bv
        bufLen = i^.iBufL&bSize
        maxPos = case im of
            InsertMode -> bufLen
            ReplaceMode -> bufLen-1
        pos = i^.iIsL.isInputL&LZ.position
        onLeftEdge  = dir == Up   && pos == 0
        onRightEdge = dir == Down && pos == dw-1
        cursor = i^.iBufL&bCursor
        nextEdge = case dir of Up -> LZ.end; Down -> LZ.beginning
        op
            | onLeftEdge  && cursor == 0      = return
            | onRightEdge && cursor == maxPos = return
            | inputValid && (onLeftEdge || onRightEdge) =
                curWord dir >=> (iIsL.isInputL %~ nextEdge) >>> return
            | onLeftEdge || onRightEdge = return
            | otherwise = iIsL.isInputL %~ LZ.move dir
                      >>> iIsL.isNewByteL .~ False
                      >>> save
    op i

insert :: Char -> Input -> InputM Input
insert c i
    | i^.iIsL.isNewByteL =
        i & iBufL %~ bInsert 0
          & iIsL.isInputL .~ LZ.singleton c
          & curHori Down
    | otherwise = do
        dw <- BV.displayWidth <$> asks icByteView
        i & iIsL.isInputL %~ (LZ.insert c >>> LZ.take dw)
          & curHori Down

replace :: Char -> Input -> InputM Input
replace c = iIsL.isInputL %~ LZ.replace c >>> curHori Down

delete :: Input -> InputM Input
delete = curHori Down >=> (iIsL.isInputL %~ LZ.pop >>> return)

remove :: Input -> InputM Input
remove i = do
    dw <- BV.displayWidth <$> asks icByteView
    let pos = i^.iIsL.isInputL & LZ.position
        cursor = i^.iBufL & bCursor
        op
            | pos == 0 && cursor == 0 = return
            | pos == 0 && dw == 1 = liftBuf Buf.removeWordPrev
            | pos == 0 = leave 
                     >=> curWord Up
                     >=> load
                     >=> iIsL.isInputL %~ (LZ.remove . LZ.end) >>> return
                     >=> iIsL.isNewByteL .~ False >>> return
                     >=> save
            | otherwise = iIsL.isInputL %~ LZ.pop >>> return
    op i
