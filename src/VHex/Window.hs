module VHex.Window
( updateWindow
) where

import Data.Word (Word8)
import Data.List (intersperse)

import Lens.Micro

import Graphics.Vty.Input.Events (Event(..), Key(..))

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.AttrMap

import VHex.Types
import VHex.Buffer
import VHex.Attributes
import VHex.Util
import VHex.ByteView (ByteView)
import qualified VHex.ByteView as BV
import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ
import VHex.ListZipper (ListZipper)
import qualified VHex.ListZipper as LZ

updateWindow :: Event -> WindowState -> EventM Name WindowState
updateWindow vtye ws = return ws

{-
updateWindow :: Event -> WindowState -> EventM Name WindowState
updateWindow vtye ws = do
    bc <- toBufferContext ws
    let bc' = case vtye of
                EvKey KLeft [] -> curHori Up bc
                EvKey KRight [] -> curHori Down bc
                _ -> bc
    return $ fromBufferContext bc' ws
-}

{-
bytesPerRow :: WindowState 
bytesPerRow b = do
    perRowMultiple <- view $ bcConfigL.cfgBytesPerRowMultipleL
    cols <- view bcColsL
    layout <- view bcLayoutL
    let offsetWidth = hexLength $ bSize b - 1
        linWidth = sum
                 $ map ((+) <$> BV.displayWidth <*> BV.spaceWidth)
                 $ LZ.toList layout
        padding = LZ.length layout - 1
        maxBytes = div (cols - offsetWidth - padding) linWidth
    return $ max 1 $ floorN perRowMultiple maxBytes where
    -}

data ByteViewContext = ByteViewContext
    { visibleBytes :: [Word8]
    , cols :: Int
    , selectedRow :: Int
    , selectedCol :: Int
    , input :: Maybe (ListZipper Char)
    }

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
