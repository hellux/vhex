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
attrDefSel     :: AttrName
attrAddr       :: AttrName
attrAddrSel    :: AttrName
attrBar        :: AttrName
attrDef         = attrName "def"
attrDefSelLine  = attrName "defSelLine"
attrDefSel      = attrName "defSel"
attrAddr        = attrName "addr"
attrAddrSel     = attrName "addrSel"
attrBar         = attrName "bar"

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

scroll :: Int -> Model e -> EventM ResourceName (Model e)
scroll n m = do
    extent <- Brick.Main.lookupExtent EditorViewPort
    case extent of
        Nothing ->
            return m
        Just (Extent _ _ (w, rows) _) -> do
            let perRow = bytesPerRow w m
            let prev = scrollPos m
            let len = (fileLength m)
            let maxPos = max 0 (len-perRow*rows)
            let newPos = min (max 0 (n*perRow+prev)) maxPos
            return m { scrollPos = newPos }

scrollBottom :: Model e -> EventM ResourceName (Model e)
scrollBottom m = do
    extent <- Brick.Main.lookupExtent EditorViewPort
    case extent of
        Nothing ->
            return m
        Just (Extent _ _ (w, rows) _) -> do
            let len = fileLength m
            return m { scrollPos = max 0 (len-(bytesPerRow w m)*rows) }

scrollTop :: Model e -> EventM ResourceName (Model e)
scrollTop m = pure $ m { scrollPos = 0 }

normalMode :: Model e -> Event -> EventM ResourceName (Next (Model e))
normalMode m vtye =
    case vtye of
        EvKey (KChar 'q') [] -> halt m
        EvKey (KChar 'y') [MCtrl] -> scroll ( -1) m >>= continue
        EvKey (KChar 'e') [MCtrl] -> scroll (  1) m >>= continue
        EvKey (KChar 'u') [MCtrl] -> scroll (-15) m >>= continue
        EvKey (KChar 'd') [MCtrl] -> scroll ( 15) m >>= continue
        EvKey (KChar 'g') [] -> scrollTop m >>= continue
        EvKey (KChar 'G') [] -> scrollBottom m >>= continue
        _ -> continue m

exCmd :: Model e -> Event -> EventM ResourceName (Next (Model e))
exCmd m vtye =
    case vtye of
        EvKey KEsc [] -> continue $ m { cmdMode = CmdNone }
        EvKey KEnter [] -> continue $ m { cmdMode = CmdNone }
        _ -> do
            cmdForm' <- handleFormEvent (VtyEvent vtye) (cmdForm m)
            continue m { cmdForm = cmdForm'}

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

{-
interleave :: [Widget ResourceName] -> [Widget ResourceName]
interleave [] = []
interleave [w] = [w]
interleave (w:ws) = w : str " " : interleave ws

-- split list into rows with given string interleaved
groupsOf :: Int -> Int -> Int -> [Word8] -> [Widget ResourceName]
groupsOf _ _ _ [] = []
groupsOf addrLength n addr ws = withAttr attrAddr (str hexAddr)
              <+> withAttr attrDef (str " ")
              <+> withAttr attrDef ((hBox . interleave) hexes)
              <+> withAttr attrBar (str "|")
              <+> withAttr attrDef (hBox asciis)
                : groupsOf addrLength n (addr+n) (drop n ws) where
    bytes = (take n ws)
    hexes = fmap (str . toHex 2 . fromIntegral) bytes
    asciis = fmap toAscii bytes
    hexAddr = toHex addrLength addr
-}

bytesPerRow :: Int -> Model e -> Int
bytesPerRow w m = maxBytes - (mod maxBytes bytesPerRowMultiple) where
    maxBytes = div (w-(hexLength (fileLength m))-1) 4

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [x] = [x]
interleave i (x:xs) = x : i : interleave i xs

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = let (y,ys) = splitAt n xs
               in y : groupsOf n ys

viewAddr :: Int -> Int -> Int -> Int -> Widget ResourceName
viewAddr start step maxAddr rowCount = vBox rows where
    addrLength = hexLength maxAddr
    rows = [ if addr <= maxAddr
                then str (toHex addrLength addr)
                else str "~"
            | r <- [0..rowCount],
              let addr = start+step*r
           ]

viewHex :: [Word8] -> Int -> Widget ResourceName
viewHex bytes perRow = vBox rows where
    rows = fmap str
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
    render $ reportExtent EditorViewPort $ hBox
        [ withAttr attrAddr
            $ padRight (Pad 2)
            $ viewAddr (scrollPos m) perRow (fileLength m) rowCount
        , withAttr attrDef
            $ viewHex bytes perRow
        , withAttr attrDef
            $ padLeft (Pad 2)
            $ viewAscii bytes perRow
        ]

{-
viewEditor :: Model e -> Widget ResourceName
viewEditor m = Widget Greedy Greedy $ do
    let contents = fileContents m
    ctx <- getContext

    let addrLength = hexLength (fileLength m)
    let perRow = bytesPerRow (ctx^.availWidthL) m
    let byteCount = perRow * ctx^.availHeightL
    let bytes = take byteCount (drop (scrollPos m) (BL.unpack contents))
    render $ reportExtent EditorViewPort
           $ vBox (groupsOf addrLength perRow (scrollPos m) bytes)
           -}

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
          , appAttrMap = const $ attrMap mempty
            [ (attrDef,          BU.fg VTY.brightWhite)
            , (attrDefSelLine,   BU.bg VTY.white)
            , (attrDefSel,       (VTY.black `BU.on` VTY.brightWhite))
            , (attrAddr,         BU.fg VTY.brightBlack)
            , (attrAddrSel,      BU.fg VTY.brightYellow)
            , (attrBar,          BU.fg VTY.brightBlack)
            ]
          }

main :: IO (Model e)
main = do
    args <- getArgs
    initial <- openFile (args !! 0) initialModel
    defaultMain app initial
