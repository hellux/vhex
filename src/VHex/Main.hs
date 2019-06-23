module VHex.Main (vhex) where

import System.IO (FilePath)
import System.Environment (getArgs)

import Data.Bits ((.&.), shiftR, shiftL)
import Data.Word (Word8)
import Data.Char (chr, ord, toLower)
import Data.List (elemIndex, intersperse)
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

data InsMode = ReplaceMode | InsertMode

data Mode = NormalMode
                CmdLineMode
          | InputMode
                InsMode
                Int     -- cursor position on selected byte

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

cursorVal :: Model -> Word8
cursorVal m = fromMaybe 0 $ Buf.selected (buffer m)

bvFocused :: Model -> ByteView
bvFocused m = (layout m) !! cursorFocus m

move :: Int -> Model -> Model
move n m = m { buffer = Buf.move n (buffer m) }

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

-- config

bytesPerRowMultiple :: Int
bytesPerRowMultiple = 4

scrollOff :: Int
scrollOff = 5

cursorLine :: Bool
cursorLine = True

-- attributes

attrDef :: AttrName
attrCursorLine :: AttrName
attrSelected :: AttrName
attrSelectedFocused :: AttrName
attrOffset :: AttrName
attrOffsetCursorLine :: AttrName
attrError :: AttrName
attrStatusBar :: AttrName
attrMode :: AttrName
attrDef = attrName "def"
attrCursorLine = attrName "cursorLine"
attrSelected = attrName "selected"
attrSelectedFocused = attrName "selectedFocused"
attrOffset = attrName "offset"
attrOffsetCursorLine = attrName "offsetCursorLine"
attrStatusBar = attrName "statusbar"
attrMode = attrName "mode"
attrError = attrName "error"

attributes :: AttrMap
attributes = attrMap mempty
    [ (attrDef, BU.fg fg)
    , (attrCursorLine, BU.bg grey23)
    , (attrSelected, BU.bg grey23 `VTY.withStyle` VTY.underline)
    , (attrSelectedFocused, bg `BU.on` fg)
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
    toWord :: String -> Maybe Word8,
    spaceWidth :: Int
}

displayWidth :: ByteView -> Int
displayWidth bv = textWidth $ fromWord bv 0

hex :: ByteView
hex = ByteView
    { fromWord = toHex 2 . fromIntegral
    , toWord = fmap fromIntegral . fromHex
    , spaceWidth = 1
    }

ascii1 :: ByteView
ascii1 = ByteView
    { fromWord = \w -> if w < 32 || w > 126
                        then "."
                        else [chr $ fromIntegral w]
    , toWord = \w -> if null w
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
    let path = filePath m
    res <- try $ BL.writeFile path contents :: IO (Either IOException ())
    case res of
        Left err -> return
            m { mode = NormalMode
                     $ CmdNone $ Just $ Left (show err) }
        Right _ -> return
            m { fileContents = contents
              , mode = NormalMode
                     $ CmdNone $ Just $ Right $ show path ++ " written"
              }

viewportSize :: EventM Name (Int, Int)
viewportSize = do
    extent <- lookupExtent EditorViewPort
    case extent of
        Nothing -> return (0, 0)
        Just (Extent _ _ dims _) -> return dims

scroll :: Int -> Model -> EventM Name Model
scroll n m = viewportSize >>= scroll' >>= keepCursor where
    scroll' (w, _) = return
        m { scrollPos = let perRow = bytesPerRow (layout m) w (bufLen m)
                            prev = scrollPos m
                            maxPos = floorN perRow (bufLen m - 1)
                        in BU.clamp 0 maxPos (prev + n*perRow) }

scrollHalfPage :: Direction -> Model -> EventM Name Model
scrollHalfPage dir m = viewportSize >>= scrollHalfPage' >>= followCursor where
    scrollHalfPage' (w, h) =
        m { scrollPos = let prev = scrollPos m
                            maxPos = floorN perRow (bufLen m - 1)
                        in BU.clamp 0 maxPos (prev + diff)
          } & moveTo newCursor & return
        where jump = (div h 2) * perRow
              diff = case dir of
                        Up -> -jump
                        Down -> jump
              perRow = bytesPerRow (layout m) w (bufLen m)
              newCursor = BU.clamp 0 (bufLen m-1) (cursorPos m+diff)

-- keep cursor in view when scrolling
keepCursor :: Model -> EventM Name Model
keepCursor m = viewportSize >>= keepCursor' where
    keepCursor' (w, h) = let perRow = bytesPerRow (layout m) w (bufLen m)
                             curRow = div (cursorPos m - scrollPos m) perRow
                             newRow = BU.clamp scrollOff (h-scrollOff-1) curRow
                             newPos = (cursorPos m) + ((newRow-curRow)*perRow)
                         in return $ moveTo newPos m

-- ensure cursor is always visible when scrolling
followCursor :: Model -> EventM Name Model
followCursor m = viewportSize >>= followCursor' where
    followCursor' (w, h) =
        let perRow = bytesPerRow (layout m) w (bufLen m)
            bottomMargin = bufLen m-1-perRow*(h-1)
            upperMargin = (cursorPos m) + perRow*(scrollOff+1-h)
            minPos = BU.clamp 0 upperMargin bottomMargin
            lowerMargin = (cursorPos m) - perRow*scrollOff
            newPos = BU.clamp minPos lowerMargin (scrollPos m)
        in return m { scrollPos = floorN perRow newPos }

goToBottom :: Model -> EventM Name Model
goToBottom m = moveTo (bufLen m - 1) m & followCursor

goToTop :: Model -> EventM Name Model
goToTop = moveTo 0 >>> followCursor

curHori :: Int -> Model -> EventM Name Model
curHori n m = m & moveTo newPos & followCursor
    where newPos = BU.clamp 0 (bufLen m - 1) (cursorPos m + n)

curVert :: Int -> Model -> EventM Name Model
curVert n m = viewportSize >>= curVert' >>= followCursor where
    curVert' (w, _) = let step = bytesPerRow (layout m) w (bufLen m)
                          newPos = min (bufLen m - 1)
                                       (cursorPos m + n*step)
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

executeCmd :: Model -> Editor String Name -> EventM Name (Next Model)
executeCmd m cmdLine = case head $ getEditContents cmdLine of
    "q" -> halt m
    "w" -> liftIO (saveFile m') >>= continue
    cmd -> continue
        m { mode = NormalMode
                 $ CmdNone $ Just $ Left ("Invalid command: " ++ cmd) }
    where
        m' = m { mode = NormalMode $ CmdNone Nothing }

updateExCmd :: Model -> Event -> Editor String Name -> EventM Name (Next Model)
updateExCmd m vtye cmdLine =
    case vtye of
        EvKey KEsc   [] -> continue $ m { mode = NormalMode $ CmdNone Nothing }
        EvKey KEnter [] -> executeCmd m cmdLine
        _ -> do
            cmdLine' <- handleEditorEvent vtye cmdLine
            continue m { mode = NormalMode $ CmdEx cmdLine' }

normalMode :: Event -> Model -> EventM Name Model
normalMode vtye = case vtye of
    EvKey (KChar 'h')  []      -> curHori (-1)
    EvKey KLeft        []      -> curHori (-1)
    EvKey (KChar 'j')  []      -> curVert 1
    EvKey KDown        []      -> curVert 1
    EvKey (KChar 'k')  []      -> curVert (-1)
    EvKey KUp          []      -> curVert (-1)
    EvKey (KChar 'l')  []      -> curHori 1
    EvKey KRight       []      -> curHori 1
    EvKey (KChar '0')  []      -> curBeginning
    EvKey (KChar '^')  []      -> curBeginning
    EvKey (KChar '$')  []      -> curEnd
    EvKey (KChar '\t') []      -> focusNext >>> return
    EvKey KBackTab     []      -> focusPrev >>> return
    EvKey (KChar 'y')  [MCtrl] -> scroll (-1)
    EvKey (KChar 'e')  [MCtrl] -> scroll 1
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
    EvKey (KChar 'X')  []      -> curHori (-1) >=> remove >>> return
    EvKey (KChar ':')  []      -> enterCmdLine >>> return
    _ -> return

enterCmdLine :: Model -> Model
enterCmdLine m = m { mode = NormalMode $ CmdEx (editor CmdLine (Just 1) "") }

enterNormalMode :: Model -> Model
enterNormalMode m = m { mode = NormalMode $ CmdNone Nothing } & moveTo newLoc
    where buf = buffer m
          newLoc = min (Buf.location buf) (Buf.length buf - 1)

enterReplaceMode :: Model -> Model
enterReplaceMode m
    | Buf.null (buffer m) = m
    | otherwise = m { mode = InputMode ReplaceMode 0 }

enterInsertMode :: Model -> Model
enterInsertMode m = m { mode = InputMode InsertMode 0 }

setChar :: ByteView -> Word8 -> Char -> Int -> Maybe Word8
setChar bv w c i = toWord bv modified
    where current = fromWord bv w
          modified = current & ix i .~ c

replaceChar :: Char -> Int -> InsMode -> Model -> EventM Name Model
replaceChar c i im m = case setChar bv selected c i of
    Nothing -> m & return
    Just w -> m & replace w & inputCurHori im i 1
    where bv = bvFocused m
          selected = cursorVal m

insertChar :: Char -> Int -> InsMode -> Model -> EventM Name Model
insertChar c i im m
    | i == 0 = case setChar (bvFocused m) 0 c i of
        Nothing -> m & return
        Just w -> insert w m & inputCurHori im i 1
    | otherwise = replaceChar c i InsertMode m

inputCurHori :: InsMode -> Int -> Int -> Model -> EventM Name Model
inputCurHori im i d m
    | i+d < 0   = m { mode = InputMode im (dw-1) } & curHori (-1)
    | i+d >= dw = let newPos = min (cursorPos m + 1) (bufLen m)
                  in m { mode = InputMode im 0 } & moveTo newPos & followCursor
    | otherwise = m { mode = InputMode im (i+d) } & return
    where dw = displayWidth (bvFocused m)

inputMode :: InsMode -> Int -> Event -> Model -> EventM Name Model
inputMode im inputCursor vtye = case vtye of
    EvKey KLeft     [] -> inputCurHori im inputCursor (-1)
    EvKey KDown     [] -> curVert 1
    EvKey KUp       [] -> curVert (-1)
    EvKey KRight    [] -> inputCurHori im inputCursor 1
    EvKey KEsc      [] -> enterNormalMode >>> return
    _ -> return

replaceMode :: Int -> Event -> Model -> EventM Name Model
replaceMode ic vtye = case vtye of
    EvKey (KChar c) [] -> replaceChar c ic ReplaceMode
    _ -> inputMode ReplaceMode ic vtye

insertMode :: Int -> Event -> Model -> EventM Name Model
insertMode ic vtye = case vtye of
    EvKey (KChar c) [] -> insertChar c ic InsertMode
    _ -> inputMode InsertMode ic vtye

update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update m e = case e of
    VtyEvent vtye -> case mode m of
        NormalMode cm -> case cm of
            CmdEx cmdLine -> updateExCmd m vtye cmdLine
            CmdNone _ -> normalMode vtye m >>= continue
        InputMode im i -> case im of
            ReplaceMode -> replaceMode i vtye m >>= continue
            InsertMode -> insertMode i vtye m >>= continue
    _ -> continue m

-- character length of hex representation of number
hexLength :: Int -> Int
hexLength = ceiling . logBase 16 . (fromIntegral :: Int -> Double)

hexChars :: String
hexChars = "0123456789abcdef"

fromHex :: String -> Maybe Int
fromHex [] = Just 0
fromHex (h:ex) = ((*) size <$> digit) >+< fromHex ex
    where (>+<) = liftM2 (+)
          digit = elemIndex (toLower h) hexChars
          size = product $ replicate (length ex) 16

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
            InputMode _ i -> showCursor Cursor (Location (i, 0)) col
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
    let focused = map ((==) (cursorFocus m)) [0..]
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
    ++ show (cursorVal m) ++ ", "
    ++ show (scrollPos m) ++ "  "

viewCmdLine :: Model -> Widget Name
viewCmdLine m = case mode m of
    NormalMode cm -> case cm of
        CmdNone Nothing -> str " "
        CmdNone (Just (Left errorMsg)) ->
            withAttr attrError (str errorMsg) <+> withAttr attrDef (str " ")
        CmdNone (Just (Right infoMsg)) ->
            withAttr attrDef (str infoMsg)
        CmdEx cmdLine -> str ":" <+> renderEditor (str . head) True cmdLine
    InputMode im _ -> case im of
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
