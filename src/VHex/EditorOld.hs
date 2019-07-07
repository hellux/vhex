module VHex.Editor (normalMode, inputMode, viewEditor) where

import Numeric (showHex)

import Data.List (intersperse)
import Data.Word (Word8)
import Data.Maybe (fromMaybe)

import Control.Monad ((>=>))
import Control.Category ((>>>))

import qualified Data.ByteString as B

import Lens.Micro ((.~), (&), (^.), ix)

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))

import Brick.Widgets.Core ( withAttr
                          , fill, str
                          , hBox, vBox
                          , showCursor
                          , reportExtent
                          , padLeft, hLimit
                          )
import Brick.Types
import Brick.Util as BU
import Brick.Main (lookupExtent)
import Brick.Widgets.Edit (editor)

import VHex.ByteView (ByteView, fromWord, toWord, spaceWidth, displayWidth)
import qualified VHex.ByteZipper as BZ
import qualified VHex.ListZipper as LZ
import VHex.Types
import VHex.Util (fromDir, floorN, hexLength, groupsOf, padOut)
import VHex.Config
import VHex.Attributes

bytesPerRow :: [ByteView] -> Int -> Int -> Int
bytesPerRow l w byteCount = max 1 $ floorN bytesPerRowMultiple maxBytes where
    offsetWidth = hexLength $ byteCount - 1
    linWidth = sum $ map ((+) <$> displayWidth <*> spaceWidth) l
    padding = length l - 1
    maxBytes = div (w - offsetWidth - padding) linWidth

editorSize :: EventM Name (Int, Int)
editorSize = do
    extent <- lookupExtent EditorViewPort
    case extent of
        Nothing -> return (0, 0)
        Just (Extent _ _ dims _) -> return dims

focusNext :: Model -> Model
focusNext m = m { cursorFocus = mod (cursorFocus m + 1) (length $ layout m) }

focusPrev :: Model -> Model
focusPrev m = m { cursorFocus = mod (cursorFocus m - 1) (length $ layout m) }

scroll :: Direction -> Model -> EventM Name Model
scroll dir m = editorSize >>= scroll' >>= keepCursor where
    scroll' (w, _) = return
        m { scrollPos = let perRow = bytesPerRow (layout m) w (bufLen m)
                            prev = scrollPos m
                            maxPos = floorN perRow (bufLen m - 1)
                        in BU.clamp 0 maxPos (prev + fromDir dir*perRow) }

scrollHalfPage :: Direction -> Model -> EventM Name Model
scrollHalfPage dir m = editorSize >>= scrollHalfPage' >>= followCursor where
    scrollHalfPage' (w, h) =
        m { scrollPos = let prev = scrollPos m
                            maxPos = floorN perRow (bufLen m - 1)
                        in BU.clamp 0 maxPos (prev + diff)
          } & moveTo newCursor & return
        where jump = div h 2 * perRow
              diff = fromDir dir * jump
              perRow = bytesPerRow (layout m) w (bufLen m)
              newCursor = BU.clamp 0 (bufLen m-1) (cursorPos m+diff)

-- keep cursor in view when scrolling
keepCursor :: Model -> EventM Name Model
keepCursor m = editorSize >>= keepCursor' where
    keepCursor' (w, h) = let perRow = bytesPerRow (layout m) w (bufLen m)
                             curRow = div (cursorPos m - scrollPos m) perRow
                             newRow = BU.clamp scrollOff (h-scrollOff-1) curRow
                             newPos = cursorPos m + ((newRow-curRow)*perRow)
                             finalPos = min (bufLen m-1) newPos
                         in return $ moveTo finalPos m

goToBottom :: Model -> EventM Name Model
goToBottom m = moveTo (bufLen m - 1) m & followCursor

goToTop :: Model -> EventM Name Model
goToTop = moveTo 0 >>> followCursor

curBeginning :: Model -> EventM Name Model
curBeginning m = editorSize >>= curBeginning' where
    curBeginning' (w, _) = let perRow = bytesPerRow (layout m) w (bufLen m)
                               newPos = floorN perRow (cursorPos m)
                           in return $ moveTo newPos m

curEnd :: Model -> EventM Name Model
curEnd m = editorSize >>= curEnd' where
    curEnd' (w, _) = let perRow = bytesPerRow (layout m) w (bufLen m)
                         lineEnd = floorN perRow (cursorPos m) + perRow - 1
                         newPos = min lineEnd (bufLen m - 1)
                     in return $ moveTo newPos m

-- ensure cursor is always visible when scrolling
followCursor :: Model -> EventM Name Model
followCursor m = editorSize >>= followCursor' where
    followCursor' (w, h) =
        let perRow = bytesPerRow (layout m) w (bufLen m)
            bottomMargin = bufLen m-1-perRow*(h-1)
            upperMargin = cursorPos m + perRow*(scrollOff+1-h)
            minPos = BU.clamp 0 upperMargin bottomMargin
            lowerMargin = cursorPos m - perRow*scrollOff
            newPos = BU.clamp minPos lowerMargin (scrollPos m)
        in return m { scrollPos = floorN perRow newPos }

curHori :: Direction -> Model -> EventM Name Model
curHori dir m = m & moveTo newPos & followCursor
    where d = fromDir dir
          maxPos = case mode m of
            InputMode im _ _ -> case im of
                InsertMode -> bufLen m
                _ -> bufLen m - 1
            _ -> bufLen m - 1
          newPos = BU.clamp 0 maxPos (cursorPos m + d)

curVert :: Direction -> Model -> EventM Name Model
curVert dir m = editorSize >>= curVert' >>= followCursor where
    curVert' (w, _) = let d = fromDir dir
                          step = bytesPerRow (layout m) w (bufLen m)
                          newPos = min (bufLen m - 1)
                                       (cursorPos m + d*step)
                          finalPos = if 0 <= newPos
                                        then newPos
                                        else cursorPos m
                      in return $ moveTo finalPos m

removePrev :: Model -> EventM Name Model
removePrev m = if cursorPos m == 0
                then m & return
                else m & curHori Up >>= (remove >>> followCursor)

normalMode :: Event -> Model -> EventM Name Model
normalMode vtye = case vtye of
    EvKey (KChar 'h')  []      -> curHori Up
    EvKey KLeft        []      -> curHori Up
    EvKey (KChar 'j')  []      -> curVert Down
    EvKey KDown        []      -> curVert Down
    EvKey (KChar 'k')  []      -> curVert Up
    EvKey KUp          []      -> curVert Up
    EvKey (KChar 'l')  []      -> curHori Down
    EvKey KRight       []      -> curHori Down
    EvKey (KChar '0')  []      -> curBeginning
    EvKey (KChar '^')  []      -> curBeginning
    EvKey (KChar '$')  []      -> curEnd
    EvKey (KChar '\t') []      -> focusNext >>> return
    EvKey KBackTab     []      -> focusPrev >>> return
    EvKey (KChar 'y')  [MCtrl] -> scroll Up
    EvKey (KChar 'e')  [MCtrl] -> scroll Down
    EvKey (KChar 'u')  [MCtrl] -> scrollHalfPage Up
    EvKey (KChar 'd')  [MCtrl] -> scrollHalfPage Down
    EvKey (KChar 'g')  []      -> goToTop
    EvKey (KChar 'G')  []      -> goToBottom
    EvKey (KChar 'R')  []      -> enterInputMode ReplaceMode >>> return
    EvKey (KChar 'i')  []      -> enterInputMode InsertMode >>> return
    EvKey (KChar 'I')  []      -> goToTop >=> enterInputMode InsertMode
                                          >>> return
    EvKey (KChar 'a')  []      -> move 1 >>> enterInputMode InsertMode
                                         >>> return
    EvKey (KChar 'A')  []      -> goToBottom >=> move 1
                                             >>> enterInputMode InsertMode
                                             >>> return
    EvKey (KChar 'x')  []      -> remove >>> return
    EvKey (KChar 'X')  []      -> removePrev
    EvKey (KChar ':')  []      -> enterCmdLine >>> return
    _ -> return

enterCmdLine :: Model -> Model
enterCmdLine m = m { mode = NormalMode $ CmdEx LZ.empty }

enterInputMode :: InsMode -> Model -> Model
enterInputMode im m
    | im == ReplaceMode && bufNull m = m
    | otherwise = m { mode = InputMode im (Input "" 0) True } & inputLoad

exitInputMode :: Model -> EventM Name Model
exitInputMode m = case mode m of
    InputMode _ (Input ip _) nb
        | nb        -> m' & curHori Up
        | ip == ""  -> m' & remove & curHori Up
        | otherwise -> m' & replace newByte & return
        where m' = m { mode = NormalMode $ CmdNone Nothing }
              current = fromMaybe 0 (cursorVal m)
              newByte = fromMaybe current $ toWord (bvFocused m) ip
    _ -> m & return

inputLoad :: Model -> Model
inputLoad m = case mode m of
    NormalMode _ -> m
    InputMode im (Input _ i) nb -> m { mode = InputMode im (Input newIp i) nb }
    where newIp = case cursorVal m of
                    Nothing -> ""
                    Just w -> fromWord (bvFocused m) w

inputSave :: Model -> Model
inputSave m = case mode m of
        InputMode _ (Input ip _) _ ->
            if ip == ""
                then m & remove & inputNewByte True & inputLoad
                else case toWord (bvFocused m) ip of
                    Nothing -> m
                    Just w -> m & replace w
        _ -> m

inputInsMode :: Model -> InsMode
inputInsMode m = case mode m of
    InputMode im _ _ -> im
    _ -> InsertMode

inputSetInput :: (String -> Int -> String) -> Model -> Model
inputSetInput f m = case mode m of
    InputMode im (Input ip i) nb ->
        m { mode = InputMode im (Input newIp i) nb }
        where dw = displayWidth (bvFocused m)
              newIp = take dw $ f ip i
    _ -> m

inputMove :: Direction -> Model -> Model
inputMove dir m = case mode m of
    InputMode im (Input ip i) _ ->
        m { mode = InputMode im (Input ip newPos) False }
        where newPos = case dir of Up -> i-1; Down -> i+1
    _ -> m

inputMoveTo :: Int -> Model -> Model
inputMoveTo i m = case mode m of
    InputMode im (Input ip _) _ ->
        m { mode = InputMode im (Input ip i) (i==0) }
    _ -> m

inputNewByte :: Bool -> Model -> Model
inputNewByte nb m = case mode m of
    InputMode im input _ -> m { mode = InputMode im input nb }
    _ -> m

inputIsNewByte :: Model -> Bool
inputIsNewByte m = case mode m of
    InputMode _ _ nb -> nb
    _ -> False

inputValid :: Model -> Bool
inputValid m = case mode m of
    InputMode _ (Input ip _) _ ->
        case toWord (bvFocused m) ip of
            Just _ -> True
            _ -> False
    _ -> False

inputCursorPos :: Model -> Int
inputCursorPos m = case mode m of
    InputMode _ (Input _ i) _ -> i
    _ -> 0

inputRemove :: Model -> EventM Name Model
inputRemove m
    | i == 0 && cursorPos m == 0 = m & return
    | i == 0 = m & inputCurHori Up
                >>= (inputLoad
                >>> inputDelete
                >>> inputSave
                >>> return)
    | otherwise = m & inputCurHori Up >>= (inputDelete >>> return)
    where i = inputCursorPos m

inputCurHori :: Direction -> Model -> EventM Name Model
inputCurHori dir m
    | onLeftEdge  && cursorPos m == 0      = m & return
    | onRightEdge && cursorPos m == maxPos = m & return
    | inputValid m && (onLeftEdge || onRightEdge) =
        m & (inputSave
         >>> curHori dir
         >=> inputMoveTo (case dir of Up -> dw-1; Down -> 0)
         >>> inputLoad
         >>> return)
    | onLeftEdge || onRightEdge = m & return
    | otherwise = m & inputMove dir
                    & inputNewByte False
                    & inputSave
                    & return
    where dw = displayWidth (bvFocused m)
          maxPos = case inputInsMode m of
                    InsertMode -> bufLen m
                    ReplaceMode -> bufLen m-1
          onLeftEdge  = dir == Up   && i == 0
          onRightEdge = dir == Down && i == dw-1
          i = inputCursorPos m

inputCurVert :: Direction -> Model -> EventM Name Model
inputCurVert dir = inputSave >>> curVert dir >=> inputLoad >>> return

inputDelete :: Model -> Model
inputDelete = inputSetInput (\ip i -> take i ip ++ drop (i+1) ip)

inputReplace :: Char -> Model -> Model
inputReplace c = inputSetInput (\ip i -> ip & ix i .~ c)

inputInsert :: Char -> Model -> Model
inputInsert c m
    | inputIsNewByte m = m & inputSetInput (\_ _ -> [c]) & insert 0
    | otherwise = m & inputSetInput (\ip i -> take i ip ++ [c] ++ drop i ip)

inputMode :: InsMode -> Event -> Model -> EventM Name Model
inputMode im vtye = case vtye of
    EvKey KLeft        [] -> inputCurHori Up
    EvKey KDown        [] -> inputCurVert Down
    EvKey KUp          [] -> inputCurVert Up
    EvKey KRight       [] -> inputCurHori Down
    EvKey KEsc         [] -> exitInputMode
    EvKey KEnter       [] -> exitInputMode
    EvKey (KChar '\t') [] -> curHori Down >=> inputMoveTo 0 >>> inputLoad
                                          >>> return
    EvKey KBackTab     [] -> curHori Up >=> inputMoveTo 0 >>> inputLoad
                                        >>> return
    EvKey KDel         [] -> inputDelete >>> return
    _ -> case im of
        ReplaceMode -> replaceMode vtye
        InsertMode -> insertMode vtye

replaceMode :: Event -> Model -> EventM Name Model
replaceMode vtye = case vtye of
    EvKey (KChar c) [] -> inputReplace c >>> inputCurHori Down
    _ -> return

insertMode :: Event -> Model -> EventM Name Model
insertMode vtye = case vtye of
    EvKey (KChar c) [] -> inputInsert c >>> inputCurHori Down
    EvKey KBS       [] -> inputRemove
    _ -> return

viewOffset :: Int -> Int -> Int -> Int -> Int -> Widget Name
viewOffset start step selected maxAddr rowCount = vBox rows where
    styleRow r = if cursorLine && r == selected
                    then attrOffsetCursorLine
                    else attrOffset
    display offset = if offset <= maxAddr
                        then showHex offset ""
                        else "~"
    rows = ( zipWith withAttr (fmap styleRow [0..rowCount-1])
           . fmap (hLimit (hexLength maxAddr) . padLeft Max . str . display)
           ) [start, start+step..]

viewBytes :: [Word8] -> Int -> (Int, Int) -> Mode -> (Bool, ByteView)
          -> Widget Name
viewBytes bytes perRow
          (selectedRow, selectedCol) mode_
          (focused, bv) = vBox rows where
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

viewEditor :: Model -> Widget Name
viewEditor m = Widget Greedy Greedy $ do
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
        $ reportExtent EditorViewPort
        $ hBox
        $ offset : views ++ [ withAttr attrDef $ fill ' ' ]
