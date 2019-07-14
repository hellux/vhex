module VHex.Window
( updateWindow
, viewWindow
) where

import Numeric (showHex)

import Data.Word (Word8)
import Data.List (intersperse)
import qualified Data.ByteString as BS

import Control.Monad.Reader

import Lens.Micro

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))

import Brick.Main
import Brick.Types
import Brick.Widgets.Core

import VHex.Types
import VHex.Attributes
import VHex.Util
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
    , start :: Int
    , cols :: Int
    , rows :: Int
    , selectedCol :: Int
    , selectedRow :: Int
    , bufLength :: Int
    , input :: Maybe (ListZipper Char)
    }

bytesPerRow :: Layout -> Int -> Int -> Int -> Int
bytesPerRow l w bprm byteCount = max 1 $ floorN bprm maxBytes where
    offsetWidth = hexLength $ byteCount - 1
    linWidth = sum
             $ map ((+) <$> BV.displayWidth <*> BV.spaceWidth)
             $ LZ.toList l
    padding = LZ.length l - 1
    maxBytes = div (w - offsetWidth - padding) linWidth

normalMode :: Mode
normalMode = NormalMode (CmdNone Nothing)

updateWindow :: Event -> EditorState -> EventM Name EditorState
updateWindow vtye es = case esMode es of
    NormalMode _ -> case vtye of
        EvKey (KChar '\t') [] -> es & esWindowL.wsLayoutL %~ LZ.rightWrap
                                    & return
        EvKey KBackTab     [] -> es & esWindowL.wsLayoutL %~ LZ.leftWrap
                                    & return
        EvKey (KChar 'i')  [] -> es & asInput InsertMode is
                                              Inp.enterInsertMode
        EvKey (KChar 'r')  [] -> es & asInput ReplaceMode is
                                              Inp.enterReplaceMode
        _                     -> es & asBuffer (normalOp vtye)
        where is = InputState { isInput = LZ.empty, isNewByte = True }
    InputMode im is -> case vtye of
        EvKey KEsc  [] -> es & asInput im is Inp.exitInputMode
                            <&> esModeL .~ normalMode
        _ -> asInput im is (inputOp vtye) es

normalOp :: Event -> (Buffer -> BufferM Buffer)
normalOp vtye = case vtye of
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

inputOp :: Event -> (Input -> InputM Input)
inputOp vtye = case vtye of
    EvKey KLeft [] -> Inp.curHori Up
    EvKey KRight [] -> Inp.curHori Down
    EvKey KUp [] -> Inp.curVert Up
    EvKey KDown [] -> Inp.curVert Down
    _ -> return

toBufCtx :: EditorState -> EventM Name BufferContext
toBufCtx es = do
    extent <- lookupExtent EditorWindow
    let (width, height) = case extent of
                Nothing -> (0, 0)
                Just (Extent _ _ dims _) -> dims
        perRow = bytesPerRow (es^.esWindowL.wsLayoutL)
                             width
                             (es^.esConfigL.cfgBytesPerRowMultipleL)
                             (es^.esWindowL.wsBufferL&BZ.length)
    return BufferContext
        { bcConfig = es^.esConfigL
        , bcRows = height
        , bcCols = perRow
        }

asBuffer :: (Buffer -> BufferM Buffer) -> EditorState -> EventM Name EditorState
asBuffer op es = do
    bc <- toBufCtx es
    let bufPrev = es & esWindow & toBuffer
        bufNext = runReader (op bufPrev) bc
    es & esWindowL %~ fromBuffer bufNext & return

asInput :: InsMode -> InputState -> (Input -> InputM Input) -> EditorState
        -> EventM Name EditorState
asInput im is op es = do
    let inpPrev = es & esWindow & toInput is
        inpNext = runReaderT (op inpPrev) ic
        ic = InputContext
            { icByteView = es^.esWindowL.wsLayoutL&LZ.selected
            , icInsMode = im
            }
    bc <- toBufCtx es
    runReader (fmap (fromInput im es) inpNext) bc & return

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

viewByteView :: ByteViewContext -> (Bool, ByteView) -> Widget Name
viewByteView bvc (focused, bv) =
    ( vBox
    . zipWith styleRow [0..]
    . map ( hBox
          . (:) (str "  ")
          . intersperse space
          . padOut (cols bvc) emptyByte
          )
    . groupsOf (cols bvc)
    . zipWith styleCol [ (div i (cols bvc), mod i (cols bvc)) | i <- [0..] ]
    . (++ [emptyByte])
    . map (str . BV.fromWord bv)
    . visibleBytes
    ) bvc
    where
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

viewWindow :: EditorState -> Widget Name
viewWindow es = Widget Greedy Greedy $ do
    ctx <- getContext
    let ws = es^.esWindowL
        buf = ws^.wsBufferL
        perRow = bytesPerRow (es^.esWindowL.wsLayoutL)
                             (ctx^.availWidthL)
                             (es^.esConfigL.cfgBytesPerRowMultipleL)
                             (es^.esWindowL.wsBufferL&BZ.length)
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
