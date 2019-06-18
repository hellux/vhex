import System.IO (FilePath)
import System.Environment (getArgs)
import System.Posix (getFileStatus, fileSize)

import qualified Data.Text as T

import Data.Bits ((.&.), shiftR, shiftL)
import Data.Word (Word8)
import Data.Char (chr)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import qualified Graphics.Vty as VTY

import Lens.Micro ((^.))

import Brick.Main
import Brick.AttrMap (AttrMap, attrMap, AttrName, attrName)
import Brick.Types
import qualified Brick.Util as BU
import Brick.Forms
import Brick.Widgets.Core

data ResourceName = EditorViewPort
                  | CmdExLine
                  | EditorBuffer
                  | HexCursor
                  deriving (Show, Eq, Ord)

{-
data Mode = NormalMode
          | InsertMode
          | ReplaceMode
          | VisualMode
-}

data CmdLineMode = CmdNone
                 | CmdEx
                 | CmdSearch
                 deriving (Show)

data ByteView = ByteView {
    fromWord :: Word8 -> String,
    toWord :: String -> Word8,
    space :: Int
}

displayWidth :: ByteView -> Int
displayWidth bv = textWidth $ (fromWord bv) 0

hex :: ByteView
hex = ByteView {
    fromWord = (toHex 2) . fromIntegral,
    toWord = undefined,
    space = 1
}

ascii1 :: ByteView
ascii1 = ByteView {
    fromWord = \w -> if w < 32 || w > 126 then "." else [chr $ fromIntegral w],
    toWord = undefined,
    space = 0
}

--data BytePart = FullByte | LowerNibble | UpperNibble

data Model e = Model
    { filePath :: FilePath
    , fileContents :: ByteString
    , fileLength :: Int
    , cursorPos :: Int              -- offset to selected byte
--    , cursorSelection :: BytePart
    , scrollPos :: Int              -- offset to visible top left byte
--    , mode :: Mode
    , cmdMode :: CmdLineMode
    , cmdForm :: Form T.Text e ResourceName
    }

bytesPerRowMultiple :: Int
bytesPerRowMultiple = 4

layout :: [ByteView]
layout = [ hex, ascii1 ]

-- attributes
attrDef :: AttrName
attrDefSelRow :: AttrName
attrDefSelCol :: AttrName
attrOffset :: AttrName
attrOffsetSel :: AttrName
attrStatusBar :: AttrName
attrDef = attrName "def"
attrDefSelRow = attrName "defSelRow"
attrDefSelCol = attrName "defSelCol"
attrOffset = attrName "offset"
attrOffsetSel = attrName "offsetSel"
attrStatusBar = attrName "statusbar"

attributes :: AttrMap
attributes = attrMap mempty
    [ (attrDef, BU.fg fg)
    , (attrDefSelRow, BU.bg grey23)
    , (attrDefSelCol, bg `BU.on` fg)
    , (attrOffset, BU.fg grey)
    , (attrOffsetSel, yellow `BU.on` grey23)
    , (attrStatusBar, BU.bg grey30)
    ] where fg = VTY.brightWhite
            bg = VTY.black
            grey = VTY.brightBlack
            yellow = VTY.brightYellow
            grey23 = VTY.rgbColor (58::Int) (58::Int) (58::Int)
            grey30 = VTY.rgbColor (78::Int) (78::Int) (78::Int)

initialModel :: Model e
initialModel = Model
    { filePath = ""
    , fileContents = BL.empty
    , fileLength = 0
    , cursorPos = 0
--    , cursorSelection = FullByte
    , scrollPos = 0
--    , mode = NormalMode
    , cmdMode = CmdNone
    , cmdForm = newForm
        [(str ":" <+>) @@= editTextField id CmdExLine (Just 1)]
        T.empty
    }

bytesPerRow :: Int -> Int -> Int
bytesPerRow w byteCount = max 1 $ floorN bytesPerRowMultiple maxBytes where
    offsetWidth = hexLength $ byteCount - 1
    linWidth = sum $ map ((+) <$> displayWidth <*> space) layout
    padding = length layout - 1
    maxBytes = div (w - offsetWidth - padding) linWidth

openFile :: FilePath -> Model e -> IO (Model e)
openFile path m = do
    contents <- BL.readFile path
    len <- fmap (fromIntegral . fileSize) (getFileStatus path)
    return m { filePath = path, fileContents = contents, fileLength = len }

viewportSize :: EventM ResourceName (Int, Int)
viewportSize = do
    extent <- lookupExtent EditorViewPort
    case extent of
        Nothing -> return (0, 0)
        Just (Extent _ _ dims _) -> return dims

scroll :: Int -> Model e -> EventM ResourceName (Model e)
scroll n m = viewportSize >>= scroll' where
    scroll' (w, _) = return
        m { scrollPos = let perRow = bytesPerRow w (fileLength m)
                            prev = scrollPos m
                            maxPos = floorN perRow (fileLength m - 1)
                        in BU.clamp 0 maxPos (prev + n*perRow) }

scrollBottom :: Model e -> EventM ResourceName (Model e)
scrollBottom m = viewportSize >>= scrollBottom' where
    scrollBottom' (w, h) = return $
        let perRow = bytesPerRow w (fileLength m)
            maxPos = fileLength m - 1
        in m { scrollPos = max 0 $ ceilN perRow (maxPos - perRow*h) }

scrollTop :: Model e -> EventM ResourceName (Model e)
scrollTop m = return m { scrollPos = 0 }

curHori :: Int -> Model e -> EventM ResourceName (Model e)
curHori n m = return
    m { cursorPos = BU.clamp 0 (fileLength m -1) ((cursorPos m) + n) }

curVert :: Int -> Model e -> EventM ResourceName (Model e)
curVert n m = viewportSize >>= curVert' where
    curVert' (w, _) = let step = bytesPerRow w (fileLength m)
                          newPos = min (fileLength m - 1) $ (cursorPos m) + n*step
                      in return
                        m { cursorPos = if 0 <= newPos
                                            then newPos
                                            else cursorPos m }

curBeginning :: Model e -> EventM ResourceName (Model e)
curBeginning m = viewportSize >>= curBeginning' where
    curBeginning' (w, _) = let perRow = bytesPerRow w (fileLength m)
                           in return
                            m { cursorPos = floorN perRow (cursorPos m) }

curEnd :: Model e -> EventM ResourceName (Model e)
curEnd m = viewportSize >>= curEnd' where
    curEnd' (w, _) = let perRow = bytesPerRow w (fileLength m)
                         lineEnd = floorN perRow (cursorPos m) + perRow - 1
                     in return
                        m { cursorPos = min lineEnd (fileLength m - 1) }

normalMode :: Model e -> Event -> EventM ResourceName (Next (Model e))
normalMode m vtye =
    case vtye of
        EvKey (KChar 'h') []      -> curHori (-1) m >>= continue
        EvKey (KChar 'j') []      -> curVert ( 1) m >>= continue
        EvKey (KChar 'k') []      -> curVert (-1) m >>= continue
        EvKey (KChar 'l') []      -> curHori ( 1) m >>= continue
        EvKey (KChar '0') []      -> curBeginning m >>= continue
        EvKey (KChar '^') []      -> curBeginning m >>= continue
        EvKey (KChar '$') []      -> curEnd m       >>= continue
        EvKey (KChar 'y') [MCtrl] -> scroll ( -1) m >>= continue
        EvKey (KChar 'e') [MCtrl] -> scroll (  1) m >>= continue
        EvKey (KChar 'u') [MCtrl] -> scroll (-15) m >>= continue
        EvKey (KChar 'd') [MCtrl] -> scroll ( 15) m >>= continue
        EvKey (KChar 'g') []      -> scrollTop m    >>= continue
        EvKey (KChar 'G') []      -> scrollBottom m >>= continue
        EvKey (KChar 'q') []      -> halt m
        _ -> continue m

exCmd :: Model e -> Event -> EventM ResourceName (Next (Model e))
exCmd m vtye =
    case vtye of
        EvKey KEsc   [] -> continue $ m { cmdMode = CmdNone }
        EvKey KEnter [] -> continue $ m { cmdMode = CmdNone }
        _ -> do
            cmdForm' <- handleFormEvent (VtyEvent vtye) (cmdForm m)
            continue m { cmdForm = cmdForm' }

update :: Model e -> BrickEvent ResourceName e
       -> EventM ResourceName (Next (Model e))
update m e =
    case e of
        VtyEvent vtye ->
            case cmdMode m of
                CmdNone ->
                    case vtye of
                        EvKey (KChar ':') [] ->
                            continue m { cmdMode = CmdEx}
                        _ ->
                            normalMode m vtye
                CmdEx -> exCmd m vtye
                CmdSearch -> continue m
        _ -> continue m

-- character length of hex representation of number
hexLength :: Int -> Int
hexLength = ceiling . (logBase 16) . (fromIntegral :: Int -> Double)

toHex :: Int -> Int -> String
toHex 0 _ = ""
toHex n d = c !! shifted : toHex (n-1) masked
    where c = "0123456789abcdef"
          s = 4*(n-1)
          shifted = (shiftR d s) .&. 0xf
          masked = d .&. ((shiftL 1 s)-1)

-- round down to closest multiple of n
floorN :: Int -> Int -> Int
floorN n x = x - (mod x n)

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

-- place i between every two elements in xs
interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [x] = [x]
interleave i (x:xs) = x : i : interleave i xs

-- split xs into lists with length n
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = let (y,ys) = splitAt n xs
               in y : groupsOf n ys

viewOffset :: Int -> Int -> Int -> Int -> Int -> Widget ResourceName
viewOffset start step selected maxAddr rowCount = vBox rows where
    styleRow r = if r == selected then attrOffsetSel else attrOffset
    display offset = if offset <= maxAddr
                        then toHex (hexLength maxAddr) offset
                        else "~"
    rows = ( zipWith withAttr (fmap styleRow [0..rowCount-1])
           . fmap str
           . fmap display
           ) [start, start+step..]

viewBytes :: [Word8] -> Int -> Int -> Int -> ByteView -> Widget ResourceName
viewBytes bytes perRow selectedRow selectedCol bv = vBox rows where
    styleCol (r, c) col = if c == selectedCol && r == selectedRow
                            then withAttr attrDefSelCol col
                            else col
    styleRow r row = let attr = if r == selectedRow
                                    then attrDefSelRow
                                    else attrDef
                     in withAttr attr row
    width = displayWidth bv
    emptyByte = str $ replicate width ' '
    rows = ( zipWith styleRow [0..]
           . fmap hBox
           . fmap (interleave (str (replicate (space bv) ' ')))
           . fmap ((str "  ") :)
           . fmap (padOut perRow emptyByte)
           . groupsOf perRow
           . zipWith styleCol [ (div i perRow, mod i perRow) | i <- [0..] ]
           . fmap (str . (fromWord bv))
           ) bytes

viewEditor :: Model e -> Widget ResourceName
viewEditor m = Widget Greedy Greedy $ do
    ctx <- getContext
    let perRow = bytesPerRow (ctx^.availWidthL) (fileLength m)
    let rowCount = ctx^.availHeightL
    let bytes = let byteCount = fromIntegral $ rowCount*perRow
                    start = fromIntegral $ scrollPos m
                    allBytes = fileContents m
                    visibleBytes = BL.take byteCount (BL.drop start allBytes)
                in BL.unpack $ visibleBytes
    let selectedRow = div ((cursorPos m) - (scrollPos m)) perRow
    let selectedCol = mod (cursorPos m) perRow
    let offset = withAttr attrOffset
               $ viewOffset (scrollPos m) perRow selectedRow
                             (fileLength m - 1) rowCount
    let views = map (viewBytes bytes perRow selectedRow selectedCol) layout
    render
        $ reportExtent EditorViewPort
        $ hBox
        $ offset : views

viewStatusBar :: Model e -> Widget ResourceName
viewStatusBar m = withAttr attrStatusBar $ str $
    filePath m ++ ": "
    ++ show (cursorPos m) ++ ", "
    ++ show (scrollPos m) ++ "  "

viewCmdLine :: Model e -> Widget ResourceName
viewCmdLine m =
    case cmdMode m of
        CmdNone -> str " "
        CmdEx -> renderForm $ cmdForm m
        CmdSearch -> str " "

view :: Model e -> [Widget ResourceName]
view m = [ viewEditor m <=> viewStatusBar m <=> viewCmdLine m ]

app :: App (Model e) e ResourceName
app = App { appDraw = view
          , appChooseCursor = showFirstCursor
          , appHandleEvent = update
          , appStartEvent = pure
          , appAttrMap = const attributes
          }

main :: IO (Model e)
main = do
    args <- getArgs
    initial <- if length args > 0
                then openFile (args !! 0) initialModel
                else return initialModel
    defaultMain app initial
