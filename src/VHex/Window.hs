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

import Graphics.Vty.Input.Events (Event(..), Key(..))

import Brick.Main
import Brick.Types
import Brick.Widgets.Core

import VHex.Types
import VHex.Buffer
import VHex.Input
import VHex.Attributes
import VHex.Util
import VHex.ByteView (ByteView)
import qualified VHex.ByteView as BV
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

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

updateWindow :: Event -> EditorState -> EventM Name EditorState
updateWindow vtye es = do
    extent <- lookupExtent EditorWindow
    let (width, height) = case extent of
                Nothing -> (0, 0)
                Just (Extent _ _ dims _) -> dims
        perRow = bytesPerRow (es^.esWindowL^.wsLayoutL)
                             width
                             (es^.esConfigL^.cfgBytesPerRowMultipleL)
                             (es^.esWindowL^.wsBufferL&BZ.length)
        bc = BufferContext
            { bcConfig = es^.esConfigL
            , bcRows = perRow
            , bcCols = height
            } 
    case esMode es of
        NormalMode _ -> return $ es & esWindowL %~ fromBuffer bufNext
            where
                op = case vtye of
                        EvKey KLeft [] -> curHori Up
                        EvKey KRight [] -> curHori Down
                        _ -> return
                bufNext = runReader (op $ toBuffer $ esWindow es) bc
        InputMode is -> return $ es & (fromInput inpNext)
            where
                inpPrev = Input
                    { iBuf = Buffer { bBuf = es^.esWindowL^.wsBufferL
                                    , bScroll = es^.esWindowL^.wsScrollPosL
                                    }
                    , iIs = is
                    }
                bv = es^.esWindowL^.wsLayoutL&LZ.selected
                ic = InputContext
                    { icFromWord = BV.fromWord bv
                    , icToWord = BV.toWord bv
                    }
                inpNext' = runReaderT (return inpPrev) ic :: BufferM Input
                inpNext = runReader inpNext' bc

viewOffset :: ByteViewContext -> Widget Name
viewOffset bvc =
    ( vBox
    . zipWith withAttr (fmap styleRow [0..rows bvc-1])
    . fmap (hLimit (hexLength (bufLength bvc)) . padLeft Max . str . display)
    ) [start bvc, start bvc+(cols bvc)..]
    where
    styleRow r = if selectedRow bvc == r
                    then attrOffsetCursorLine
                    else attrOffset
    display offset = if offset <= (bufLength bvc)
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
        let attr = if r == (selectedRow bvc)
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
        perRow = bytesPerRow (es^.esWindowL^.wsLayoutL)
                             (ctx^.availWidthL)
                             (es^.esConfigL^.cfgBytesPerRowMultipleL)
                             (BZ.length buf)
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
                        InputMode is -> Just (is^.isInputL)
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
