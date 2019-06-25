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

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import qualified Graphics.Vty as VTY

import Lens.Micro ((.~), (&), (^.), ix)

import Brick.Main
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Types
import qualified Brick.Util as BU
import Brick.Widgets.Core
import Brick.Widgets.Edit

import VHex.Buffer (Buffer)
import qualified VHex.Buffer as Buf

data Name = EditorViewPort
          | CmdLine
          | EditorBuffer
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
data InsMode = ReplaceMode
             | InsertMode

data Mode = NormalMode CmdLineMode
          | InputMode
                InsMode
                Input
                Bool -- entered new byte

data Model = Model
    { filePath :: FilePath
    , fileContents :: ByteString
    , buffer :: Buffer
    , layout :: [ByteView]
    , cursorFocus :: Int            -- focused view by index
    , scrollPos :: Int              -- offset to visible top left byte
    , mode :: Mode
    }

bufLen :: Model -> Int
bufLen = Buf.length . buffer

cursorPos :: Model -> Int
cursorPos m = Buf.location (buffer m)

cursorVal :: Model -> Maybe Word8
cursorVal m = Buf.selected (buffer m)

bvFocused :: Model -> ByteView
bvFocused m = layout m !! cursorFocus m

move :: Int -> Model -> Model
move n m = if cursorPos m < bufLen m
            then m { buffer = Buf.move n (buffer m) }
            else m

moveTo :: Int -> Model -> Model
moveTo i m = m { buffer = Buf.moveTo i (buffer m) }

replace :: Word8 -> Model -> Model
replace w m = if Buf.null (buffer m)
    then m
    else m { buffer = Buf.replace w (buffer m) }

insert :: Word8 -> Model -> Model
insert w m = m { buffer = Buf.insert w (buffer m) }

remove :: Model -> Model
remove m = if Buf.null (buffer m)
    then m
    else m { buffer = Buf.remove (buffer m) }

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
attrStatusBar :: AttrName
attrMode :: AttrName
attrCurrent = attrName "current"
attrDef = attrName "def"
attrCursorLine = attrName "cursorLine"
attrSelected = attrName "selected"
attrSelectedFocused = attrName "selectedFocused"
attrInvalid = attrName "invalid"
attrOffset = attrName "offset"
attrOffsetCursorLine = attrName "offsetCursorLine"
attrStatusBar = attrName "statusbar"
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
    , (attrStatusBar, BU.bg grey30)
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
trim = dropWhileEnd isSpace . dropWhile isSpace

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
                        else [chr $ fromIntegral w]
    , _toWord = \w -> if null w
                        then Just 0
                        else (Just . fromIntegral . ord . head) w
    , spaceWidth = 0
    }

initialModel :: Model
initialModel = Model
    { filePath = ""
    , fileContents = BL.empty
    , buffer = Buf.empty
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
    contents <- BL.readFile path
    return m { filePath = path
             , fileContents = contents
             , buffer = Buf.buffer contents
             }

saveFile:: Model -> IO Model
saveFile m = do
    let contents = Buf.contents (buffer m)
        path = filePath m
    res <- try $ BL.writeFile path contents :: IO (Either IOException ())
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
          newPos = BU.clamp 0 (bufLen m - 1) (cursorPos m + d)

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

exitInputMode :: Input -> Bool -> Model -> Model
exitInputMode (Input ip _) nb m = m { mode = NormalMode $ CmdNone Nothing }
                                    & (if nb
                                        then id
                                        else replace newByte)
                                    & moveTo newPos
    where current = fromMaybe 0 (cursorVal m)
          newByte = fromMaybe current $ toWord (bvFocused m) ip
          newPos = min (bufLen m-1) (cursorPos m) :: Int

enterReplaceMode :: Model -> Model
enterReplaceMode m
    | Buf.null (buffer m) = m
    | otherwise = m { mode = InputMode ReplaceMode (Input "" 0) True }
                    & updateInput

enterInsertMode :: Model -> Model
enterInsertMode m =
    m { mode = InputMode InsertMode (Input "" 0) True }
                & updateInput

replaceChar :: Char -> Input -> Model -> EventM Name Model
replaceChar c (Input ip i) m = m & inputCurHori ReplaceMode newInput Down
    where newInput = Input (ip & ix i .~ c) i

insertChar :: Bool -> Char -> Input -> Model -> EventM Name Model
insertChar isNew c (Input ip i) m
    | isNew = m & inputCurHori InsertMode (Input [c] i) Down
            >>= (insert 0 >>> return)
    | otherwise = if length newIp > dw
        then m & return
        else m & inputCurHori InsertMode (Input newIp i) Down
    where newIp = take i ip ++ [c] ++ drop i ip
          dw = displayWidth (bvFocused m) 

updateInput :: Model -> Model
updateInput m = case mode m of
    NormalMode _ -> m
    InputMode im (Input _ i) nb -> m { mode = InputMode im (Input newIp i) nb }
    where newIp = case cursorVal m of
                    Nothing -> ""
                    Just w -> fromWord (bvFocused m) w

inputCurHori :: InsMode -> Input -> Direction -> Model -> EventM Name Model
inputCurHori im input@(Input ip i) dir m
    | dir == Up && i == 0 && cursorPos m == 0 = m & return
    | dir == Up && i == 0 = case newByte of
        Nothing -> m { mode = InputMode im input False } & return
        Just w -> m { mode = InputMode im (Input ip (dw-1)) True }
                    & replace w
                    & curHori Up
                    >>= (updateInput >>> return)
    | dir == Down && i == dw-1 = case newByte of
        Nothing -> m { mode = InputMode im input False } & return
        Just w -> m { mode = InputMode im (Input ip 0) True }
                    & replace w
                    & case im of
                        ReplaceMode -> curHori Down
                        InsertMode -> (move 1 >>> return)
                    >>= (updateInput >>> followCursor)
    | otherwise =
        m { mode = InputMode im (Input ip (fromDir dir + i)) False } & return
    where bv = bvFocused m
          dw = displayWidth bv
          newByte = toWord bv ip

inputCurVert :: Input -> Direction -> Model -> EventM Name Model
inputCurVert (Input ip _) dir m = case newByte of
    Nothing -> m & return
    Just w -> m & replace w & curVert dir >>= (updateInput >>> return)
    where bv = bvFocused m
          newByte = toWord bv ip

inputMode :: InsMode -> Input -> Bool -> Event -> Model -> EventM Name Model
inputMode im input nb vtye = case vtye of
    EvKey KLeft     [] -> inputCurHori im input Up
    EvKey KDown     [] -> inputCurVert input Down
    EvKey KUp       [] -> inputCurVert input Up
    EvKey KRight    [] -> inputCurHori im input Down
    EvKey KEsc      [] -> exitInputMode input nb >>> return
    _ -> return

replaceMode :: Input -> Bool -> Event -> Model -> EventM Name Model
replaceMode input nb vtye = case vtye of
    EvKey (KChar c) [] -> replaceChar c input 
    _ -> inputMode ReplaceMode input nb vtye

insertMode :: Input -> Bool -> Event -> Model -> EventM Name Model
insertMode input nb vtye = case vtye of
    EvKey (KChar c) [] -> insertChar nb c input
    EvKey KBS       [] -> removePrev -- TODO handle individual chars
    _ -> inputMode InsertMode input nb vtye

update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update m (VtyEvent vtye) = case mode m of
    NormalMode cm -> case cm of
        CmdEx cmdLine -> updateExCmd m vtye cmdLine
        CmdNone _ -> normalMode vtye m >>= continue
    InputMode im input nb -> case im of
        ReplaceMode -> replaceMode input nb vtye m >>= continue
        InsertMode -> insertMode input nb vtye m >>= continue
update m _ = continue m

-- character length of hex representation of number
hexLength :: Int -> Int
hexLength = ceiling . logBase 16 . (fromIntegral :: Int -> Double)

hexChars :: String
hexChars = "0123456789abcdef"

fromHex :: String -> Maybe Int
fromHex [] = Just 0
fromHex (h:ex) = fmap (*size) digit >+< fromHex ex
    where (>+<) = liftM2 (+) :: Maybe Int -> Maybe Int -> Maybe Int
          digit = elemIndex (toLower h) hexChars :: Maybe Int
          size = 16^length ex

toHex :: Int -> Int -> String
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
    let bytes = BL.unpack
              $ Buf.slice (scrollPos m) (rowCount*perRow) (buffer m)
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

viewStatusBar :: Model -> Widget Name
viewStatusBar m = withAttr attrStatusBar $ str $
    filePath m ++ ": "
    ++ show (cursorPos m) ++ ", "
    ++ show (scrollPos m) ++ "  "

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
view m = [ viewEditor m <=> viewStatusBar m <=> viewCmdLine m ]

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
