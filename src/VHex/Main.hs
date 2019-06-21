module VHex.Main (vhex) where

import System.IO (FilePath)
import System.Environment (getArgs)
--import System.Posix (getFileStatus, fileSize)

import Data.Bits ((.&.), shiftR, shiftL)
import Data.Word (Word8)
import Data.Char (chr, ord, toLower)
import Data.List (elemIndex, intersperse)
import Data.Maybe (fromMaybe)

import Control.Monad (liftM2, (>=>))
import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Exception --(try, IOException(..))

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

data Mode = NormalMode
          | ReplaceMode
          | InsertMode

data CmdLineMode = CmdNone (Maybe (Either String String))
                 | CmdEx (Editor String Name)
--               | CmdSearch
                 deriving (Show)

data Model = Model
    { filePath :: FilePath
    , fileContents :: ByteString
    , buffer :: Buffer
    , cursorFocus :: Int            -- focused view by index
    , scrollPos :: Int              -- offset to visible top left byte
    , inputCursor :: Maybe Int -- TODO mv to mode
    , mode :: Mode
    , cmdMode :: CmdLineMode
    }

bufferLength :: Model -> Int
bufferLength = Buf.length . buffer

cursorPos :: Model -> Int
cursorPos m = fromMaybe 0 $ Buf.selectedIndex (buffer m)

cursorVal :: Model -> Word8
cursorVal m = fromMaybe 0 $ Buf.selectedValue (buffer m)

bvFocused :: Model -> ByteView
bvFocused m = layout !! cursorFocus m

bytesPerRowMultiple :: Int
bytesPerRowMultiple = 4

enableCursorLine :: Bool
enableCursorLine = True

layout :: [ByteView]
layout = [ hex, ascii1 ] -- TODO mv to model

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
    , cursorFocus = 0
    , scrollPos = 0
    , inputCursor = Nothing
    , mode = NormalMode
    , cmdMode = CmdNone Nothing
    }

bytesPerRow :: Int -> Int -> Int
bytesPerRow w byteCount = max 1 $ floorN bytesPerRowMultiple maxBytes where
    offsetWidth = hexLength $ byteCount - 1
    linWidth = sum $ map ((+) <$> displayWidth <*> spaceWidth) layout
    padding = length layout - 1
    maxBytes = div (w - offsetWidth - padding) linWidth

openFile :: FilePath -> Model -> IO Model
openFile path m = do
    contents <- BL.readFile path
    --len <- fmap (fromIntegral . fileSize) (getFileStatus path)
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
            m { cmdMode = CmdNone $ Just $ Left (show err) }
        Right _ -> return
            m { fileContents = contents
              , cmdMode = CmdNone $ Just $ Right $ show path ++ " written"
              }

viewportSize :: EventM Name (Int, Int)
viewportSize = do
    extent <- lookupExtent EditorViewPort
    case extent of
        Nothing -> return (0, 0)
        Just (Extent _ _ dims _) -> return dims

scroll :: Int -> Model -> EventM Name Model
scroll n m = viewportSize >>= scroll' where
    scroll' (w, _) = return
        m { scrollPos = let perRow = bytesPerRow w (bufferLength m)
                            prev = scrollPos m
                            maxPos = floorN perRow (bufferLength m - 1)
                        in BU.clamp 0 maxPos (prev + n*perRow) }

scrollBottom :: Model -> EventM Name Model
scrollBottom m = viewportSize >>= scrollBottom' where
    scrollBottom' (w, h) = return $
        let perRow = bytesPerRow w (bufferLength m)
            maxPos = bufferLength m - 1
        in m { scrollPos = max 0 $ ceilN perRow (maxPos - perRow*h) }

scrollTop :: Model -> EventM Name Model
scrollTop m = return m { scrollPos = 0 }

curHori :: Int -> Model -> EventM Name Model
curHori n m = return m { buffer = Buf.moveTo newPos (buffer m) }
    where newPos = BU.clamp 0 (bufferLength m - 1) (cursorPos m + n)

curVert :: Int -> Model -> EventM Name Model
curVert n m = viewportSize >>= curVert' where
    curVert' (w, _) = let step = bytesPerRow w (bufferLength m)
                          newPos = min (bufferLength m - 1)
                                       (cursorPos m + n*step)
                          finalPos = if 0 <= newPos
                                        then newPos
                                        else cursorPos m
                      in return
                        m { buffer = Buf.moveTo finalPos (buffer m) }

curBeginning :: Model -> EventM Name Model
curBeginning m = viewportSize >>= curBeginning' where
    curBeginning' (w, _) = let perRow = bytesPerRow w (bufferLength m)
                               newPos = floorN perRow (cursorPos m)
                           in return
                            m { buffer = Buf.moveTo newPos (buffer m) }

curEnd :: Model -> EventM Name Model
curEnd m = viewportSize >>= curEnd' where
    curEnd' (w, _) = let perRow = bytesPerRow w (bufferLength m)
                         lineEnd = floorN perRow (cursorPos m) + perRow - 1
                         newPos = min lineEnd (bufferLength m - 1)
                     in return
                        m { buffer = Buf.moveTo newPos (buffer m) }

focusNext :: Model -> EventM Name Model
focusNext m = return
    m { cursorFocus = mod (cursorFocus m + 1) (length layout) }

focusPrev :: Model -> EventM Name Model
focusPrev m = return
    m { cursorFocus = mod (cursorFocus m - 1) (length layout) }

executeCmd :: Model -> Editor String Name
           -> EventM Name (Next Model)
executeCmd m cmdLine = case head $ getEditContents cmdLine of
    "q" -> halt m
    "w" -> liftIO (saveFile m') >>= continue
    cmd -> continue
        m { cmdMode = CmdNone $ Just $ Left ("Invalid command: " ++ cmd) }
    where
        m' = m { cmdMode = CmdNone Nothing }

updateExCmd :: Model -> Event -> Editor String Name -> EventM Name (Next Model)
updateExCmd m vtye cmdLine =
    case vtye of
        EvKey KEsc   [] -> continue $ m { cmdMode = CmdNone Nothing }
        EvKey KEnter [] -> executeCmd m cmdLine
        _ -> do
            cmdLine' <- handleEditorEvent vtye cmdLine
            continue m { cmdMode = CmdEx cmdLine' }

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
    EvKey (KChar '\t') []      -> focusNext
    EvKey KBackTab     []      -> focusPrev
    EvKey (KChar 'y')  [MCtrl] -> scroll (-1)
    EvKey (KChar 'e')  [MCtrl] -> scroll 1
    EvKey (KChar 'u')  [MCtrl] -> scroll (-15)
    EvKey (KChar 'd')  [MCtrl] -> scroll 15
    EvKey (KChar 'g')  []      -> scrollTop
    EvKey (KChar 'G')  []      -> scrollBottom
    EvKey (KChar 'r')  []      -> enterReplaceMode >>> return
    EvKey (KChar 'i')  []      -> enterInsertMode >>> return
    -- TODO fix append behaviour on last char
    EvKey (KChar 'a')  []      -> curHori 1 >=> enterInsertMode >>> return
    EvKey (KChar 'x')  []      -> removeSelection >>> return
    EvKey (KChar 'X')  []      -> curHori (-1) >=> removeSelection >>> return
    EvKey (KChar ':')  []      -> enterCmdLine >>> return
    _ -> return

enterCmdLine :: Model -> Model
enterCmdLine m = m { cmdMode = CmdEx (editor CmdLine (Just 1) "") }

enterNormalMode :: Model -> Model
enterNormalMode m = m { mode = NormalMode
                      , inputCursor = Nothing
                      , cmdMode = CmdNone Nothing -- clear info/error msg
                      }

enterReplaceMode :: Model -> Model
enterReplaceMode m
    | Buf.null (buffer m) = m
    | otherwise = m { mode = ReplaceMode, inputCursor = Just 0 }

enterInsertMode :: Model -> Model
enterInsertMode m = m { mode = InsertMode, inputCursor = Just 0 }

setSelection :: Model -> Word8 -> Model
setSelection m w = if Buf.null (buffer m)
    then m
    else m { buffer = Buf.replace w (buffer m) }

insertSelection :: Model -> Word8 -> Model
insertSelection m w = m { buffer = Buf.insert w (buffer m) }

removeSelection :: Model -> Model
removeSelection m = if Buf.null (buffer m)
    then m
    else m { buffer = Buf.remove (buffer m) }

setChar :: ByteView -> Word8 -> Char -> Int -> Maybe Word8
setChar bv w c i = toWord bv modified
    where current = fromWord bv w
          modified = current & ix i .~ c

replaceChar :: Char -> Model -> EventM Name Model
replaceChar c m =
    case inputCursor m >>= replaceChar' of
        Nothing -> return m
        Just m' -> inputCurHori 1 m'
    where bv = bvFocused m
          replaceChar' = fmap (setSelection m) . setChar bv (cursorVal m) c

insertChar :: Char -> Model -> EventM Name Model
insertChar c m =
    case inputCursor m of
        Just 0 -> case setChar (bvFocused m) 0 c 0 of
            Nothing -> return m
            Just newWord -> inputCurHori 1 (insertSelection m newWord)
        _ -> replaceChar c m

-- XXX d must be (-1) or 1, maybe use custom data type?
inputCurHori :: Int -> Model -> EventM Name Model
inputCurHori d m = case inputCursor m of
        Nothing -> return m
        Just i | i+d < 0   -> curHori d m { inputCursor = Just (dw-1) }
               | i+d >= dw -> curHori d m { inputCursor = Just 0 }
               | otherwise -> return m { inputCursor = Just (i+d) }
        where dw = displayWidth (bvFocused m)

inputCurVert :: Int -> Model -> EventM Name Model
inputCurVert = curVert

inputMode :: Event -> Model -> EventM Name Model
inputMode vtye = case vtye of
    EvKey KLeft     [] -> inputCurHori (-1)
    EvKey KDown     [] -> inputCurVert 1
    EvKey KUp       [] -> inputCurVert (-1)
    EvKey KRight    [] -> inputCurHori 1
    EvKey KEsc      [] -> enterNormalMode >>> return
    _ -> return

replaceMode :: Event -> Model -> EventM Name Model
replaceMode vtye = case vtye of
    EvKey (KChar c) [] -> replaceChar c
    _ -> inputMode vtye

insertMode :: Event -> Model -> EventM Name Model
insertMode vtye = case vtye of
    EvKey (KChar c) [] -> insertChar c
    _ -> inputMode vtye

update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update m e = case e of
    VtyEvent vtye -> case mode m of
        NormalMode -> case cmdMode m of
            CmdEx cmdLine -> updateExCmd m vtye cmdLine
            CmdNone _ -> normalMode vtye m >>= continue
        ReplaceMode -> replaceMode vtye m >>= continue
        InsertMode -> insertMode vtye m >>= continue
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

-- round up to closest multiple of n
ceilN :: Int -> Int -> Int
ceilN n x
    | mod x n == 0 = x
    | otherwise    = floorN n x + n

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
    styleRow r = if enableCursorLine && r == selected
                    then attrOffsetCursorLine
                    else attrOffset
    display offset = if offset <= maxAddr
                        then toHex (hexLength maxAddr) offset
                        else "~"
    rows = ( zipWith withAttr (fmap styleRow [0..rowCount-1])
           . fmap (str . display)
           ) [start, start+step..]

viewBytes :: [Word8] -> Int -> (Int, Int) -> Maybe Int -> (Bool, ByteView)
          -> Widget Name
viewBytes bytes perRow
          (selectedRow, selectedCol) ic
          (focused, bv) = vBox rows where
    styleCol (r, c) col
        | c /= selectedCol || r /= selectedRow = col
        | not focused = withAttr attrSelected col
        | otherwise = case ic of
            Nothing -> withAttr attrSelectedFocused col
            Just i -> showCursor Cursor (Location (i, 0)) col
    styleRow r row =
        let attr = if enableCursorLine && r == selectedRow
                    then attrCursorLine
                    else attrDef
        in withAttr attr row
    emptyByte = str $ replicate (displayWidth bv) ' '
    space     = str $ replicate (spaceWidth bv) ' '
    rows = ( zipWith styleRow [0..]
           . fmap ( hBox
                  . (:) (str "  ")
                  . intersperse space
                  . padOut perRow emptyByte
                  )
           . groupsOf perRow
           . zipWith styleCol [ (div i perRow, mod i perRow) | i <- [0..] ]
           . fmap (str . fromWord bv)
           ) bytes

viewEditor :: Model -> Widget Name
viewEditor m = Widget Greedy Greedy $ do
    ctx <- getContext
    let perRow = bytesPerRow (ctx^.availWidthL) (bufferLength m)
    let rowCount = ctx^.availHeightL
    let bytes = let byteCount = fromIntegral $ rowCount*perRow
                    start = fromIntegral $ scrollPos m
                    visibleBytes = Buf.slice start byteCount (buffer m)
                in BL.unpack visibleBytes
    let selectedRow = div (cursorPos m - scrollPos m) perRow
    let selectedCol = mod (cursorPos m) perRow
    let offset = withAttr attrOffset
               $ viewOffset (scrollPos m) perRow selectedRow
                            (bufferLength m - 1) rowCount
    let focused = map (\i -> i == cursorFocus m) [0..] :: [Bool]
    let views = map (viewBytes bytes perRow
                               (selectedRow, selectedCol) (inputCursor m))
                    (zip focused layout)
    render
        $ reportExtent EditorViewPort
        $ hBox
        $ offset : views ++ [ withAttr attrDef $ fill ' ' ]

viewStatusBar :: Model -> Widget Name
viewStatusBar m = withAttr attrStatusBar $ str $
    filePath m ++ ": "
    ++ show (cursorPos m) ++ ", "
    ++ show (scrollPos m) ++ "  "
    ++ case inputCursor m of
        Nothing -> ""
        Just i -> show i

viewCmdLine :: Model -> Widget Name
viewCmdLine m =
    case cmdMode m of
        CmdNone errorMsg ->
            case mode m of
                NormalMode  -> case errorMsg of
                    Nothing -> str " "
                    Just msg -> case msg of
                        Left err -> withAttr attrError (str err)
                                    <+> withAttr attrDef (str " ")
                        Right info -> withAttr attrDef (str info)
                ReplaceMode -> withAttr attrMode $ str "-- REPLACE --"
                InsertMode  -> withAttr attrMode $ str "-- INSERT --"
        CmdEx cmdLine -> str ":" <+> renderEditor (str . head) True cmdLine
--      CmdSearch -> str " "

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
    initial <- if not (null args)
                then openFile (head args) initialModel
                else return initialModel
    _ <- defaultMain app initial
    return ()