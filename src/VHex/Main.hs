module VHex.Main (vhex) where

import System.IO (FilePath)
import System.Environment (getArgs)

import Data.Bits ((.&.), shiftR, shiftL)
import Data.Word (Word8)
import Data.Char (chr, ord, toLower, isSpace)
import Data.List (elemIndex, intersperse, dropWhileEnd)
import Data.Maybe (fromMaybe)

import Control.Monad (liftM2, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Category ((>>>))
import Control.Exception (try, IOException)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import qualified Graphics.Vty as VTY

import Lens.Micro ((.~), (&), (^.), ix)

import Brick.Main
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Types
import qualified Brick.Util as BU
import Brick.Widgets.Core
import Brick.Widgets.Edit

import VHex.ByteZipper (ByteZipper)
import qualified VHex.ByteZipper as BZ

data Name = EditorViewPort
          | CmdLine
          | Cursor
          deriving (Show, Eq, Ord)

data CmdLineMode = CmdNone
                    (Maybe
                        (Either
                            String   -- error message
                            String)) -- info message
                 | CmdEx (Editor String Name)
                 deriving (Show)

data Input = Input
                String  -- string representation of byte
                Int     -- cursor position on selected byte
            deriving (Show)
data InsMode = ReplaceMode
             | InsertMode
             deriving (Show)

data Mode = NormalMode CmdLineMode
          | InputMode
                InsMode
                Input
                Bool -- entered new byte

data Model = Model
    { filePath :: FilePath
    , fileContents :: ByteString
    , buffer :: ByteZipper
    , layout :: [ByteView]
    , cursorFocus :: Int            -- focused view by index
    , scrollPos :: Int              -- offset to visible top left byte
    , mode :: Mode
    }

bvFocused :: Model -> ByteView
bvFocused m = layout m !! cursorFocus m

bufLen :: Model -> Int
bufLen = BZ.length . buffer

cursorPos :: Model -> Int
cursorPos m = BZ.location (buffer m)

cursorVal :: Model -> Maybe Word8
cursorVal m = BZ.selected (buffer m)

moveTo :: Int -> Model -> Model
moveTo i m = let i' = BU.clamp 0 (bufLen m) i
             in m { buffer = BZ.moveTo i' (buffer m) }

move :: Int -> Model -> Model
move n m = m & moveTo (cursorPos m+n)

replace :: Word8 -> Model -> Model
replace w m = m { buffer = BZ.replace w (buffer m) }

insert :: Word8 -> Model -> Model
insert w m = m { buffer = BZ.insert w (buffer m) }

remove :: Model -> Model
remove m = if BZ.null (buffer m)
    then m
    else m { buffer = BZ.remove (buffer m) }

removePrev :: Model -> EventM Name Model
removePrev m = if cursorPos m == 0
                then m & return
                else m & curHori Up >>= (remove >>> followCursor)

-- config

bytesPerRowMultiple :: Int
bytesPerRowMultiple = 4

scrollOff :: Int
scrollOff = 5

cursorLine :: Bool
cursorLine = True

-- attributes

attrCurrent :: AttrName
attrDef :: AttrName
attrCursorLine :: AttrName
attrSelected :: AttrName
attrSelectedFocused :: AttrName
attrInvalid :: AttrName
attrOffset :: AttrName
attrOffsetCursorLine :: AttrName
attrError :: AttrName
attrStatusLine :: AttrName
attrMode :: AttrName
attrCurrent = attrName "current"
attrDef = attrName "def"
attrCursorLine = attrName "cursorLine"
attrSelected = attrName "selected"
attrSelectedFocused = attrName "selectedFocused"
attrInvalid = attrName "invalid"
attrOffset = attrName "offset"
attrOffsetCursorLine = attrName "offsetCursorLine"
attrStatusLine = attrName "statusline"
attrMode = attrName "mode"
attrError = attrName "error"

attributes :: AttrMap
attributes = attrMap mempty
    [ (attrCurrent, VTY.currentAttr)
    , (attrDef, BU.fg fg)
    , (attrCursorLine, BU.bg grey23)
    , (attrSelected, BU.bg grey23 `VTY.withStyle` VTY.underline)
    , (attrSelectedFocused, bg `BU.on` fg)
    , (attrInvalid, red `BU.on` grey23)
    , (attrOffset, BU.fg grey)
    , (attrOffsetCursorLine, yellow `BU.on` grey23)
    , (attrStatusLine, BU.bg grey30)
    , (attrMode, BU.fg yellow `VTY.withStyle` VTY.bold)
    , (attrError, grey15 `BU.on` red `VTY.withStyle` VTY.bold)
    ] where fg = VTY.brightWhite
            bg = VTY.black
            red = VTY.brightRed
            grey = VTY.brightBlack
            yellow = VTY.brightYellow
            grey15 = VTY.rgbColor (38::Int) (38::Int) (38::Int)
            grey23 = VTY.rgbColor (58::Int) (58::Int) (58::Int)
            grey30 = VTY.rgbColor (78::Int) (78::Int) (78::Int)

data ByteView = ByteView {
    fromWord :: Word8 -> String,
    _toWord :: String -> Maybe Word8,
    spaceWidth :: Int
}

trim :: String -> String
trim " " = " "
trim s = (dropWhileEnd isSpace . dropWhile isSpace) s

toWord :: ByteView -> String -> Maybe Word8
toWord bv = _toWord bv . trim

displayWidth :: ByteView -> Int
displayWidth bv = textWidth $ fromWord bv 0

hex :: ByteView
hex = ByteView
    { fromWord = toHex 2 . fromIntegral
    , _toWord = fmap fromIntegral . fromHex
    , spaceWidth = 1
    }

ascii1 :: ByteView
ascii1 = ByteView
    { fromWord = \w -> if w < 32 || w > 126
                        then "."
                        else (return . chr . fromIntegral) w
    , _toWord = \w -> case w of
                        "" -> Just 0
                        c:_ -> (Just . fromIntegral . ord ) c
    , spaceWidth = 0
    }

initialModel :: Model
initialModel = Model
    { filePath = ""
    , fileContents = B.empty
    , buffer = BZ.empty
    , layout = [ hex, ascii1 ]
    , cursorFocus = 0
    , scrollPos = 0
    , mode = NormalMode (CmdNone Nothing)
    }

bytesPerRow :: [ByteView] -> Int -> Int -> Int
bytesPerRow l w byteCount = max 1 $ floorN bytesPerRowMultiple maxBytes where
    offsetWidth = hexLength $ byteCount - 1
    linWidth = sum $ map ((+) <$> displayWidth <*> spaceWidth) l
    padding = length l - 1
    maxBytes = div (w - offsetWidth - padding) linWidth

openFile :: FilePath -> Model -> IO Model
openFile path m = do
    contents <- B.readFile path
    return m { filePath = path
             , fileContents = contents
             , buffer = BZ.byteZipper contents
             }

saveFile:: Model -> IO Model
saveFile m = do
    let contents = BZ.contents (buffer m)
        path = filePath m
    res <- try $ B.writeFile path contents :: IO (Either IOException ())
    case res of
        Left err -> m & errorMsg (show err) & return
        Right _ -> m { fileContents = contents }
                    & infoMsg (show path ++ " written")
                    & return

viewportSize :: EventM Name (Int, Int)
viewportSize = do
    extent <- lookupExtent EditorViewPort
    case extent of
        Nothing -> return (0, 0)
        Just (Extent _ _ dims _) -> return dims

fromDir :: Direction -> Int
fromDir Up = -1
fromDir Down = 1

scroll :: Direction -> Model -> EventM Name Model
scroll dir m = viewportSize >>= scroll' >>= keepCursor where
    scroll' (w, _) = return
        m { scrollPos = let perRow = bytesPerRow (layout m) w (bufLen m)
                            prev = scrollPos m
                            maxPos = floorN perRow (bufLen m - 1)
                        in BU.clamp 0 maxPos (prev + fromDir dir*perRow) }

scrollHalfPage :: Direction -> Model -> EventM Name Model
scrollHalfPage dir m = viewportSize >>= scrollHalfPage' >>= followCursor where
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
keepCursor m = viewportSize >>= keepCursor' where
    keepCursor' (w, h) = let perRow = bytesPerRow (layout m) w (bufLen m)
                             curRow = div (cursorPos m - scrollPos m) perRow
                             newRow = BU.clamp scrollOff (h-scrollOff-1) curRow
                             newPos = cursorPos m + ((newRow-curRow)*perRow)
                             finalPos = min (bufLen m-1) newPos
                         in return $ moveTo finalPos m

-- ensure cursor is always visible when scrolling
followCursor :: Model -> EventM Name Model
followCursor m = viewportSize >>= followCursor' where
    followCursor' (w, h) =
        let perRow = bytesPerRow (layout m) w (bufLen m)
            bottomMargin = bufLen m-1-perRow*(h-1)
            upperMargin = cursorPos m + perRow*(scrollOff+1-h)
            minPos = BU.clamp 0 upperMargin bottomMargin
            lowerMargin = cursorPos m - perRow*scrollOff
            newPos = BU.clamp minPos lowerMargin (scrollPos m)
        in return m { scrollPos = floorN perRow newPos }

goToBottom :: Model -> EventM Name Model
goToBottom m = moveTo (bufLen m - 1) m & followCursor

goToTop :: Model -> EventM Name Model
goToTop = moveTo 0 >>> followCursor

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
curVert dir m = viewportSize >>= curVert' >>= followCursor where
    curVert' (w, _) = let d = fromDir dir
                          step = bytesPerRow (layout m) w (bufLen m)
                          newPos = min (bufLen m - 1)
                                       (cursorPos m + d*step)
                          finalPos = if 0 <= newPos
                                        then newPos
                                        else cursorPos m
                      in return $ moveTo finalPos m

curBeginning :: Model -> EventM Name Model
curBeginning m = viewportSize >>= curBeginning' where
    curBeginning' (w, _) = let perRow = bytesPerRow (layout m) w (bufLen m)
                               newPos = floorN perRow (cursorPos m)
                           in return $ moveTo newPos m

curEnd :: Model -> EventM Name Model
curEnd m = viewportSize >>= curEnd' where
    curEnd' (w, _) = let perRow = bytesPerRow (layout m) w (bufLen m)
                         lineEnd = floorN perRow (cursorPos m) + perRow - 1
                         newPos = min lineEnd (bufLen m - 1)
                     in return $ moveTo newPos m

focusNext :: Model -> Model
focusNext m = m { cursorFocus = mod (cursorFocus m + 1) (length $ layout m) }

focusPrev :: Model -> Model
focusPrev m = m { cursorFocus = mod (cursorFocus m - 1) (length $ layout m) }

errorMsg :: String -> Model -> Model
errorMsg msg m = m { mode = NormalMode $ CmdNone $ Just $ Left msg }

infoMsg :: String -> Model -> Model
infoMsg msg m = m { mode = NormalMode $ CmdNone $ Just $ Right msg }

executeCmd :: String -> Model -> EventM Name (Next Model)
executeCmd "write" m = liftIO (saveFile m') >>= continue
    where m' = m { mode = NormalMode $ CmdNone Nothing }
executeCmd "quit" m = halt m
executeCmd "w" m = m & executeCmd "w"
executeCmd "q" m = executeCmd "quit" m
executeCmd cmd m = m & errorMsg ("Invalid command: " ++ cmd) & continue

updateExCmd :: Model -> Event -> Editor String Name -> EventM Name (Next Model)
updateExCmd m vtye cmdLine =
    case vtye of
        EvKey KEsc   [] -> continue $ m { mode = NormalMode $ CmdNone Nothing }
        EvKey KEnter [] -> executeCmd (head $ getEditContents cmdLine) m
        _ -> do
            cmdLine' <- handleEditorEvent vtye cmdLine
            continue m { mode = NormalMode $ CmdEx cmdLine' }

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
    EvKey (KChar 'R')  []      -> enterReplaceMode >>> return
    EvKey (KChar 'i')  []      -> enterInsertMode >>> return
    EvKey (KChar 'I')  []      -> goToTop >=> enterInsertMode >>> return
    EvKey (KChar 'a')  []      -> move 1 >>> enterInsertMode >>> return
    EvKey (KChar 'A')  []      -> goToBottom >=>
                                    move 1 >>> enterInsertMode >>> return
    EvKey (KChar 'x')  []      -> remove >>> return
    EvKey (KChar 'X')  []      -> removePrev
    EvKey (KChar ':')  []      -> enterCmdLine >>> return
    _ -> return

enterCmdLine :: Model -> Model
enterCmdLine m = m { mode = NormalMode $ CmdEx (editor CmdLine (Just 1) "") }

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

enterReplaceMode :: Model -> Model
enterReplaceMode m
    | BZ.null (buffer m) = m
    | otherwise = m { mode = InputMode ReplaceMode (Input "" 0) True }
                    & inputLoad

enterInsertMode :: Model -> Model
enterInsertMode m =
    m { mode = InputMode InsertMode (Input "" 0) True }
                & inputLoad

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
        m & inputMoveTo (case dir of Up -> dw-1; Down -> 0)
          & inputSave
          & curHori dir
          >>= (inputLoad >>> followCursor)
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

update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update m (VtyEvent vtye) = case mode m of
    NormalMode cm -> case cm of
        CmdEx cmdLine -> updateExCmd m vtye cmdLine
        CmdNone _ -> normalMode vtye m >>= continue
    InputMode im _ _ -> inputMode im vtye m >>= continue
update m _ = continue m

-- character length of hex representation of number
hexLength :: Int -> Int
hexLength = max 1 . ceiling . logBase 16 . (fromIntegral :: Int -> Double)

hexChars :: String
hexChars = "0123456789abcdef"

fromHex :: String -> Maybe Int
fromHex [] = Just 0
fromHex (h:ex) = fmap (*size) digit >+< fromHex ex
    where (>+<) = liftM2 (+) :: Maybe Int -> Maybe Int -> Maybe Int
          digit = elemIndex (toLower h) hexChars :: Maybe Int
          size = 16^length ex

toHex :: Int -> Int -> String
toHex n _ | n < 0 = error "hex length must be non-negative"
toHex _ d | d < 0 = error "hex number must be non-negative"
toHex 0 _ = ""
toHex n d = hexChars !! shifted : toHex (n-1) masked
    where s = 4*(n-1)
          shifted = shiftR d s .&. 0xf
          masked = d .&. (shiftL 1 s - 1)

-- round down to closest multiple of n
floorN :: Int -> Int -> Int
floorN n x = x - mod x n

-- fill xs with p until length is n
padOut :: Int -> a -> [a] -> [a]
padOut n p xs
    | len < n = xs ++ replicate (n-len) p
    | otherwise = xs
    where len = length xs

-- split xs into lists with length n
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = let (y,ys) = splitAt n xs
                in y : groupsOf n ys

viewOffset :: Int -> Int -> Int -> Int -> Int -> Widget Name
viewOffset start step selected maxAddr rowCount = vBox rows where
    styleRow r = if cursorLine && r == selected
                    then attrOffsetCursorLine
                    else attrOffset
    display offset = if offset <= maxAddr
                        then toHex (hexLength maxAddr) offset
                        else "~"
    rows = ( zipWith withAttr (fmap styleRow [0..rowCount-1])
           . fmap (str . display)
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

viewStatusLine :: Model -> Widget Name
viewStatusLine m = withAttr attrStatusLine $ str $
    filePath m ++ ": "
    ++ show (cursorPos m) ++ ", "
    ++ show (bufLen m) ++ "  "
    ++ case mode m of
        NormalMode _ -> ""
        InputMode im input nb ->
            "Input " ++ show im ++ " " ++ show input ++ " " ++ show nb

viewCmdLine :: Model -> Widget Name
viewCmdLine m = case mode m of
    NormalMode cm -> case cm of
        CmdNone Nothing -> str " "
        CmdNone (Just (Left err)) ->
            withAttr attrError (str err) <+> withAttr attrDef (str " ")
        CmdNone (Just (Right info)) ->
            withAttr attrDef (str info)
        CmdEx cmdLine -> str ":" <+> renderEditor (str . head) True cmdLine
    InputMode im _ _ -> case im of
        ReplaceMode -> withAttr attrMode $ str "-- REPLACE --"
        InsertMode -> withAttr attrMode $ str "-- INSERT --"

view :: Model -> [Widget Name]
view m = [ viewEditor m <=> viewStatusLine m <=> viewCmdLine m ]

app :: App Model e Name
app = App { appDraw = view
          , appChooseCursor = showFirstCursor
          , appHandleEvent = update
          , appStartEvent = pure
          , appAttrMap = const attributes
          }

vhex :: IO ()
vhex = do
    args <- getArgs
    model <- if not (null args)
                then initialModel & openFile (head args)
                else initialModel & return
    _ <- defaultMain app model
    return ()
