module VHex.Editor (updateWindow) where

import Graphics.Vty.Input.Events (Event(..), Key(..))
import Brick.Types

import VHex.Types
import VHex.State.Buffer
    ( toBufferContext, fromBufferContext
    , curHori
    )

updateWindow :: Event -> WindowState -> EventM Name WindowState
updateWindow vtye ws = do
    bc <- toBufferContext ws
    let bc' = case vtye of
                EvKey KLeft [] -> curHori Up bc
                EvKey KRight [] -> curHori Down bc
                _ -> bc
    return $ fromBufferContext bc' ws

viewBytes :: WindowState -> [Word8] -> (Bool, ByteView) -> Widget Name
viewBytes ws bytes (focused, bv) = vBox rows where
    styleCol (r, c) col
        | c /= selectedCol || r /= selectedRow = col
        | not focused = withAttr attrSelected col
        | otherwise = case mode_ of
            NormalMode _ -> withAttr attrSelectedFocused col
            InputMode _ (Input ip i) _ ->
                let attr = case toWord bv ip of
                            Nothing -> attrInvalid
                            Just _ -> attrCurrent
                in withAttr attr
                    $ showCursor Cursor (Location (i, 0))
                    $ str
                    $ padOut (displayWidth bv) ' ' ip
    styleRow r row =
        let attr = if cursorLine && r == selectedRow
                    then attrCursorLine
                    else attrDef
        in withAttr attr row
    emptyByte = str $ replicate (displayWidth bv) ' '
    space     = str $ replicate (spaceWidth bv) ' '
    rows = ( zipWith styleRow [0..]
           . map ( hBox
                 . (:) (str "  ")
                 . intersperse space
                 . padOut perRow emptyByte
                 )
           . groupsOf perRow
           . zipWith styleCol [ (div i perRow, mod i perRow) | i <- [0..] ]
           . (++ [emptyByte])
           . map (str . fromWord bv)
           ) bytes

viewWindow :: Model -> Widget Name
viewWindow m = Widget Greedy Greedy $ do
    ctx <- getContext
    let perRow = bytesPerRow (layout m) (ctx^.availWidthL) (bufLen m)
    let rowCount = ctx^.availHeightL
    let bytes = B.unpack
              $ BZ.slice (scrollPos m) (rowCount*perRow) (buffer m)
    let selectedRow = div (cursorPos m - scrollPos m) perRow
    let selectedCol = mod (cursorPos m) perRow
    let offset = withAttr attrOffset
               $ viewOffset (scrollPos m) perRow selectedRow
                            (bufLen m - 1) rowCount
    let focused = map (cursorFocus m ==) [0..]
    let views = map (viewBytes bytes perRow
                               (selectedRow, selectedCol) (mode m))
                    (zip focused $ layout m)
    render
        $ reportExtent EditorWindow
        $ hBox
        $ offset : views ++ [ withAttr attrDef $ fill ' ' ]
