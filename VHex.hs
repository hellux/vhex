import System.IO (FilePath)
import System.Environment (getArgs)
import System.Posix (getFileStatus, fileSize)

import qualified Data.Text as T

import Data.Bits ((.&.), shiftR, shiftL)
import Data.Word (Word8)
import Data.Char (chr)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Graphics.Vty.Input.Events
import qualified Graphics.Vty as VTY

import Lens.Micro ((^.))

import Brick.Main
import Brick.AttrMap
import Brick.Forms
import Brick.Types
import qualified Brick.Util as BU
import Brick.Widgets.Core

data ResourceName = EditorViewPort
                  | CmdExLine
                  | EditorBuffer
                  | HexCursor
                  deriving (Show, Eq, Ord)

data CmdLineMode = CmdNone
                 | CmdEx
                 | CmdSearch
                 deriving (Show)

data Model e = Model
    { filePath :: FilePath
    , fileContents :: ByteString
    , fileLength :: Int
    , cursorPos :: Int
    , scrollPos :: Int
    , cmdMode :: CmdLineMode
    , cmdForm :: Form T.Text e ResourceName
    }

bytesPerRowMultiple :: Int
bytesPerRowMultiple = 4

-- attributes
attrDef        :: AttrName
attrDefSelLine :: AttrName
attrAddr       :: AttrName
attrAddrSel    :: AttrName
attrDef         = attrName "def"
attrDefSelLine  = attrName "defSelLine"
attrAddr        = attrName "addr"
attrAddrSel     = attrName "addrSel"

attributes :: AttrMap
attributes = attrMap mempty
    [ (attrDef,        BU.fg fg)
    , (attrDefSelLine, BU.bg grey23)
    , (attrAddr,       BU.fg grey)
    , (attrAddrSel,    yellow `BU.on` grey23)
    ] where grey23 = VTY.rgbColor (58::Int) (58::Int) (58::Int)
            fg = VTY.brightWhite
            grey = VTY.brightBlack
            yellow = VTY.brightYellow

initialModel :: Model e
initialModel = Model
    { filePath = ""
    , fileContents = BL.empty
    , fileLength = 0
    , cmdMode = CmdNone
    , cmdForm = newForm
        [(str ":" <+>) @@= editTextField id CmdExLine (Just 1)]
        T.empty
    , cursorPos = 0
    , scrollPos = 0
    }

openFile :: FilePath -> Model e -> IO (Model e)
openFile path m = do
    contents <- BL.readFile path
    len <- fmap (fromIntegral . fileSize) (getFileStatus path)
    return m { filePath = path, fileContents = contents, fileLength = len }

viewportDims :: EventM ResourceName (Int, Int)
viewportDims = do
    extent <- lookupExtent EditorViewPort
    case extent of
        Nothing -> return (0, 0)
        Just (Extent _ _ dims _) -> return dims

-- TODO replace scrollPos with line no
scroll :: Int -> Model e -> EventM ResourceName (Model e)
scroll n m = viewportDims >>= scroll' where
    scroll' (w, h) = return
        m { scrollPos = let perRow = bytesPerRow w m
                            prev = scrollPos m
                            maxPos = max 0 ((fileLength m) - perRow*h)
                        in min (max 0 (n*perRow+prev)) maxPos }

scrollBottom :: Model e -> EventM ResourceName (Model e)
scrollBottom m = viewportDims >>= scrollBottom' where
    scrollBottom' (w, h) = return
        m { scrollPos = max 0 $ (fileLength m) - (bytesPerRow w m)*h }

scrollTop :: Model e -> EventM ResourceName (Model e)
scrollTop m = return m { scrollPos = 0 }

curHori :: Int -> Model e -> EventM ResourceName (Model e)
curHori n m = return m { cursorPos = (cursorPos m) + n }

curVert :: Int -> Model e -> EventM ResourceName (Model e)
curVert n m = viewportDims >>= curRight' where
    curRight' (w, _) = let step = bytesPerRow w m
                       in return m { cursorPos = (cursorPos m) + step*n }

normalMode :: Model e -> Event -> EventM ResourceName (Next (Model e))
normalMode m vtye =
    case vtye of
        EvKey (KChar 'h') []      -> curHori (-1) m >>= continue
        EvKey (KChar 'j') []      -> curVert ( 1) m >>= continue
        EvKey (KChar 'k') []      -> curVert (-1) m >>= continue
        EvKey (KChar 'l') []      -> curHori ( 1) m >>= continue
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

hexLength :: Int -> Int
hexLength = ceiling . (logBase 16) . (fromIntegral :: Int -> Double)

toHex :: Int -> Int -> String
toHex 0 _ = ""
toHex n d = c !! shifted : toHex (n-1) masked
    where c = "0123456789abcdef"
          s = 4*(n-1)
          shifted = (shiftR d s) .&. 0xf
          masked = d .&. ((shiftL 1 s)-1)

toAscii :: Word8 -> String
toAscii w
    | w < 32 || w > 126 = "."
    | otherwise = [chr $ fromIntegral w]

bytesPerRow :: Int -> Model e -> Int
bytesPerRow w m = max 1 $ maxBytes - (mod maxBytes bytesPerRowMultiple) where
    maxBytes = div (w-(hexLength (fileLength m))-1) 4

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [x] = [x]
interleave i (x:xs) = x : i : interleave i xs

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = let (y,ys) = splitAt n xs
               in y : groupsOf n ys

viewAddr :: Int -> Int -> Int -> Int -> Int -> Widget ResourceName
viewAddr start step selected maxAddr rowCount = vBox rows where
    addrLength = hexLength maxAddr
    rows = [ withAttr attr
             $ str
             $ if addr <= maxAddr
                then (toHex addrLength addr)
                else "~"
            | r <- [0..rowCount],
              let addr = start+step*r,
              let attr = if r == selected then attrAddrSel else attrAddr
           ]

viewHex :: [Word8] -> Int -> Int -> Widget ResourceName
viewHex bytes perRow selectedRow = vBox rows where
    widgetize r row = let attr = if r == selectedRow
                                    then attrDefSelLine
                                    else attrDef
                      in withAttr attr (str row)
    rows = zipWith widgetize [0..]
         $ fmap (concat . (interleave " "))
         $ groupsOf perRow (fmap ((toHex 2) . fromIntegral) bytes)

viewAscii :: [Word8] -> Int -> Widget ResourceName
viewAscii bytes perRow = vBox rows where
    rows = fmap str
         $ fmap concat
         $ groupsOf perRow (fmap toAscii bytes)

viewEditor :: Model e -> Widget ResourceName
viewEditor m = Widget Greedy Greedy $ do
    ctx <- getContext
    let perRow = bytesPerRow (ctx^.availWidthL) m
    let rowCount = ctx^.availHeightL
    let bytes = let byteCount = fromIntegral $ rowCount*perRow
                    start = fromIntegral $ scrollPos m
                    allBytes = fileContents m
                    visibleBytes = BL.take byteCount (BL.drop start allBytes)
                in BL.unpack $ visibleBytes
    let selectedRow = div ((cursorPos m) - (scrollPos m)) perRow
    render $ reportExtent EditorViewPort $ hBox
        [ withAttr attrAddr
            $ padRight (Pad 2)
            $ viewAddr (scrollPos m)
                       perRow
                       selectedRow
                       (fileLength m)
                       rowCount
        , withAttr attrDef
            $ viewHex bytes perRow selectedRow
        , withAttr attrDef
            $ padLeft (Pad 2)
            $ viewAscii bytes perRow
        ]

viewCmdLine :: Model e -> Widget ResourceName
viewCmdLine m =
    case cmdMode m of
        CmdNone -> str $ filePath m ++ ": "
                      ++ show (cursorPos m) ++ ", "
                      ++ show (scrollPos m)
        CmdEx -> renderForm $ cmdForm m
        CmdSearch -> str " "

view :: Model e -> [Widget ResourceName]
view m = [
    padBottom Max (viewEditor m) <=>
    viewCmdLine m
    ]

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
    initial <- openFile (args !! 0) initialModel
    defaultMain app initial
