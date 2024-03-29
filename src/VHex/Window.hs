{-|
Module      : VHex.Main
Description : The editor window.
Copyright   : (c) Noah Hellman, 2019
License     : GPL-3
Maintainer  : noah.hellman@protonmail.com
Stability   : unstable
Portability : not portable
-}

module VHex.Window
( updateWindow
, viewWindow
) where

import Numeric (showHex)

import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Data.List (intersperse)
import qualified Data.ByteString as BS
import Data.Function (on)

import Control.Monad ((>=>))
import Control.Monad.Reader (runReader, runReaderT)
import Control.Category ((>>>))

import Lens.Micro ((^.), (&), (%~), (.~))

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))

import Brick.Types
import Brick.Main (lookupExtent, invalidateCacheEntry, invalidateCache)
import Brick.Widgets.Core ( hBox, vBox
                          , showCursor
                          , reportExtent
                          , cached
                          , hLimit, padLeft
                          , withAttr
                          , fill, str
                          )

import VHex.Types
import VHex.Attributes
import VHex.Util (groupsOf, hexLength, padOut, floorN)
import VHex.Command (commandMode)
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

import VHex.Window.Buffer
    ( BufferM
    , BufferContext(..)
    , Buffer(..)
    , toBuffer, fromBuffer
    )
import qualified VHex.Window.Buffer as Buf
import VHex.Window.Input
    ( InputM
    , InputContext(..)
    , Input(..)
    , toInput, fromInput
    )
import qualified VHex.Window.Input as Inp
import VHex.Window.ByteView (ByteView)
import qualified VHex.Window.ByteView as BV

data ByteViewContext = ByteViewContext
    { visibleBytes :: [Word8]
    -- ^ All bytes that are visible.
    , start :: Int
    -- ^ Offset to first visible byte.
    , cols :: Int
    -- ^ Number of bytes per row.
    , rows :: Int
    -- ^ Number of rows of bytes.
    , selectedCol :: Int
    -- ^ Selected column, 0-indexed.
    , selectedRow :: Int
    -- ^ Selected row, 0-index from top of screen.
    , bufLength :: Int
    -- ^ Total size of buffer in bytes.
    , input :: Maybe (ListZipper Char)
    -- ^ User byte input.
    }

-- | Determine how many bytes should be shown per row. Depends on the size of
-- the offset bar, the number of frames and their widths.
bytesPerRow :: Int -> EditorState -> Int
bytesPerRow w es = max 1 (floorN bprm maxBytes) where
    bprm = es&esConfig&cfgBytesPerRowMultiple
    layout = es&esWindow&wsLayout
    offsetWidth = hexLength $ (es&esWindow&wsBuffer&BZ.length) - 1
    linWidth = sum
             $ map ((+) <$> BV.displayWidth <*> BV.spaceWidth)
             $ LZ.toList layout
    padding = LZ.length layout - 1
    maxBytes = div (w - offsetWidth - padding) linWidth

-- | Number of columns and rows of the editor window.
dimensions :: EditorState -> EventM Name (Int, Int)
dimensions es = do
    extent <- lookupExtent EditorWindow
    let (width, height) = case extent of
                            Nothing -> (0, 0)
                            Just (Extent _ _ dims _) -> dims
        perRow = bytesPerRow width es
    return (perRow, height)

normalMode :: Mode
normalMode = NormalMode (CmdNone Nothing)

updateWindow :: Event -> EditorState -> EventM Name EditorState
updateWindow vtye esPrev = do
    let op = case esMode esPrev of
            NormalMode _ -> normalModeOp
            InputMode im is -> inputModeOp im is
    esNext <- op vtye esPrev

    if ((==) `on` (BZ.length . wsBuffer . esWindow)) esPrev esNext
        then do
            (w, _) <- dimensions esPrev
            let bvs = esPrev & esWindow & wsLayout & LZ.toList
                invalidateRow r =
                    mapM_ (invalidateCacheEntry . (`CachedRow` r)) bvs
                rowPrev = floorN w (esPrev & esWindow & wsBuffer & BZ.location)
            invalidateRow rowPrev
        else invalidateCache

    return esNext

-- | Get operation that operates in normal mode.
normalModeOp :: Event -> EditorState -> EventM Name EditorState
normalModeOp vtye = case vtye of
    EvKey (KChar '\t') [] -> esWindowL.wsLayoutL %~ LZ.rightWrap >>> return
    EvKey KBackTab     [] -> esWindowL.wsLayoutL %~ LZ.leftWrap >>> return
    EvKey (KChar 'i')  [] -> asInput InsertMode is Inp.enterInputMode
    EvKey (KChar 'a')  [] -> asInput InsertMode is Inp.enterInputModeAppend
    EvKey (KChar 'I')  [] -> asBuffer Buf.curTop
                         >=> asInput InsertMode is Inp.enterInputMode
    EvKey (KChar 'A')  [] -> asBuffer Buf.curBottom
                         >=> asInput InsertMode is Inp.enterInputModeAppend
    EvKey (KChar 'R')  [] -> asInput ReplaceMode is Inp.enterInputMode
    EvKey (KChar ':')  [] -> esModeL .~ commandMode >>> return
    _                     -> asBuffer (bufferOp vtye)
    where is = InputState { isInput = LZ.empty, isNewByte = True }

-- | Get operation that operates in input mode.
inputModeOp :: InsMode -> InputState
            -> Event -> EditorState -> EventM Name EditorState
inputModeOp im is vtye = case vtye of
    EvKey KEsc   [] -> asInput im is Inp.exitInputMode
                   >=> (esModeL .~ normalMode >>> return)
    EvKey KEnter [] -> asInput im is Inp.exitInputMode
                   >=> (esModeL .~ normalMode >>> return)
    _ -> asInput im is (inputOp im vtye)

-- | Get operation that operates directly on buffer.
bufferOp :: Event -> (Buffer -> BufferM Buffer)
bufferOp vtye = case vtye of
    EvKey (KChar 'h') [] -> Buf.curHori Up
    EvKey KLeft [] -> Buf.curHori Up
    EvKey (KChar 'j') [] -> Buf.curVert Down
    EvKey KDown [] -> Buf.curVert Down
    EvKey (KChar 'k') [] -> Buf.curVert Up
    EvKey KUp [] -> Buf.curVert Up
    EvKey (KChar 'l') [] -> Buf.curHori Down
    EvKey KRight [] -> Buf.curHori Down
    EvKey (KChar '0') [] -> Buf.curBeginning
    EvKey (KChar '^') [] -> Buf.curBeginning
    EvKey (KChar '$') [] -> Buf.curEnd
    EvKey (KChar 'g') [] -> Buf.curTop
    EvKey (KChar 'G') [] -> Buf.curBottom
    EvKey (KChar 'y') [MCtrl] -> Buf.scroll Up
    EvKey (KChar 'e') [MCtrl] -> Buf.scroll Down
    EvKey (KChar 'u') [MCtrl] -> Buf.scrollHalfPage Up
    EvKey (KChar 'd') [MCtrl] -> Buf.scrollHalfPage Down
    EvKey (KChar 'x') [] -> Buf.removeWord
    EvKey (KChar 'X') [] -> Buf.removeWordPrev
    _ -> return

-- | Get operation that operates directly on input.
inputOp :: InsMode -> Event -> (Input -> InputM Input)
inputOp im vtye = case vtye of
    EvKey KLeft [] -> Inp.curHori Up
    EvKey KRight [] -> Inp.curHori Down
    EvKey KUp [] -> Inp.curVert Up
    EvKey KDown [] -> Inp.curVert Down
    EvKey (KChar '\t') [] -> Inp.curWord Down
    EvKey KBackTab [] -> Inp.curWord Up
    _ -> case im of
        InsertMode -> case vtye of
            EvKey KBS [] -> Inp.remove
            EvKey KDel [] -> Inp.delete
            EvKey (KChar c) [] -> Inp.insert c
            _ -> return
        ReplaceMode -> case vtye of
            EvKey (KChar c) [] -> Inp.replace c
            _ -> return

toBufCtx :: EditorState -> EventM Name BufferContext
toBufCtx es = do
    (w, h) <- dimensions es
    return BufferContext
        { bcConfig = es^.esConfigL
        , bcCols = w
        , bcRows = h
        }

-- | Apply an operation in the context of a Buffer.
asBuffer :: (Buffer -> BufferM Buffer) -> EditorState
         -> EventM Name EditorState
asBuffer op es = do
    bc <- toBufCtx es

    let bufPrev = es & esWindow & toBuffer
        bufNext = runReader (op bufPrev) bc

    es & esWindowL %~ fromBuffer bufNext & return

-- | Apply an operation in the context of an Input.
asInput :: InsMode -> InputState -> (Input -> InputM Input) -> EditorState
        -> EventM Name EditorState
asInput im is op es = do
    let bv = fromMaybe BV.hex (es^.esWindowL.wsLayoutL&LZ.selected)
        ic = InputContext
            { icByteView = bv
            , icInsMode = im
            }
        inpPrev = es & esWindow & toInput is
        inpNext = runReaderT (op inpPrev) ic

    bc <- toBufCtx es
    runReader (fmap (fromInput im es) inpNext) bc & return

-- | Create widget of offsets stacked on top of each other starting from top.
viewOffset :: ByteViewContext -> Widget Name
viewOffset bvc =
    ( vBox
    . zipWith withAttr (fmap styleRow [0..rows bvc-1])
    . fmap (hLimit (hexLength (bufLength bvc)) . padLeft Max . str . display)
    ) [start bvc, start bvc+cols bvc..]
    where
    styleRow r = if selectedRow bvc == r
                    then attrOffsetCursorLine
                    else attrOffset
    display offset = if offset <= bufLength bvc
                        then showHex offset ""
                        else "~"

-- | Create a widget for a frame with provided byteview.
viewByteView :: ByteViewContext -> (Bool, ByteView) -> Widget Name
viewByteView bvc (focused, bv) =
    ( vBox
    . zipWith styleRow [0..]
    . zipWith maybeCached [0..]
    . map ( hBox
          . (:) (str "  ")
          . intersperse space
          . padOut (cols bvc) emptyByte
          )
    . groupsOf (cols bvc)
    . zipWith styleCol rowsAndCols
    . map (str . BV.fromWord bv)
    . visibleBytes
    ) bvc
    where
    maybeCached :: Int -> Widget Name -> Widget Name
    maybeCached row = let offset = start bvc + (cols bvc*row)
                      in if row == selectedRow bvc
                            then id
                            else cached (CachedRow bv offset)
    rowsAndCols = [ (div i (cols bvc), mod i (cols bvc)) | i <- [0..] ]
    styleCol pos col
        | pos /= (selectedRow bvc, selectedCol bvc) = col
        | not focused = withAttr attrSelected col
        | otherwise = case input bvc of
            Nothing -> withAttr attrSelectedFocused col
            Just inp ->
                let attr = case BV.toWord bv (LZ.toList inp) of
                            Nothing -> attrInvalid
                            Just _ -> attrCurrent
                in withAttr attr
                    $ showCursor InputCursor (Location (LZ.position inp, 0))
                    $ str
                    $ padOut (BV.displayWidth bv) ' ' (LZ.toList inp)
    styleRow r row =
        let attr = if r == selectedRow bvc
                    then attrCursorLine
                    else attrDef
        in withAttr attr row
    emptyByte = str $ replicate (BV.displayWidth bv) ' '
    space     = str $ replicate (BV.spaceWidth bv) ' '

-- | Create a widget for the editor window.
viewWindow :: EditorState -> Widget Name
viewWindow es = Widget Greedy Greedy $ do
    ctx <- getContext
    let ws = es^.esWindowL
        buf = ws^.wsBufferL
        perRow = bytesPerRow (ctx^.availWidthL) es
        bvc = ByteViewContext
            { visibleBytes = BS.unpack
                           $ BZ.slice (ws^.wsScrollPosL)
                                      (ctx^.availHeightL*perRow) buf
            , start = ws^.wsScrollPosL
            , rows = ctx^.availHeightL
            , cols = perRow
            , selectedRow = div (BZ.location buf - ws^.wsScrollPosL) perRow
            , selectedCol = mod (BZ.location buf) perRow
            , bufLength = BZ.length buf
            , input = case es^.esModeL of
                        InputMode _ is -> Just (is^.isInputL)
                        _ -> Nothing
            }
        offset = viewOffset bvc
        focused = map (LZ.position (ws^.wsLayoutL) ==) [0..]
        views = map (viewByteView bvc)
                    (zip focused $ LZ.toList $ ws^.wsLayoutL)
    render
        $ reportExtent EditorWindow
        $ hBox
        $ offset : views ++ [ withAttr attrDef $ fill ' ' ]
