import System.IO (FilePath)
import System.Environment (getArgs)

import qualified Data.Text as T

import Data.Bits ((.&.), shiftR, shiftL)
import Data.Word (Word8)
import Data.Char (chr)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Graphics.Vty.Input.Events

import Lens.Micro ((^.))

import Brick.Main
import Brick.AttrMap
import Brick.Forms
import Brick.Types
import Brick.Widgets.Core

data ResourceName = EditorViewPort
                  | CmdLineFormRes
                  | EditorBuffer
                  deriving (Show, Eq, Ord)

data CmdLineMode = CmdNone
                 | CmdEx
                 | CmdSearch
                 deriving (Show)

data Model e = Model
    { filePath :: FilePath
    , fileContents :: ByteString
    , cursorPos :: Int
    , scrollPos :: Int
    , cmdMode :: CmdLineMode
    , cmdForm :: Form T.Text e ResourceName
    }

initialModel :: FilePath -> IO (Model e)
initialModel fname = do
    contents <- BL.readFile fname
    return $ Model
        { filePath = fname
        , fileContents = contents
        , cmdMode = CmdNone
        , cmdForm = (mkExForm $ T.empty)
        , cursorPos = 0
        , scrollPos = 0
        }

-- TODO change pos by n rows
scroll :: Int -> Model e -> Model e
scroll n m = m { scrollPos = newPos } where
    prev = scrollPos m
    len = fromIntegral (BL.length (fileContents m)-1)
    newPos = min (max 0 (n+prev)) len

normalMode :: Model e -> Event -> EventM ResourceName (Next (Model e))
normalMode m vtye =
    case vtye of
        EvKey (KChar 'q') [] -> halt m
        EvKey (KChar 'y') [MCtrl] -> continue (scroll ( -1) m)
        EvKey (KChar 'e') [MCtrl] -> continue (scroll (  1) m)
        EvKey (KChar 'u') [MCtrl] -> continue (scroll (-15) m)
        EvKey (KChar 'd') [MCtrl] -> continue (scroll ( 15) m)
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

mkExForm :: T.Text -> Form T.Text e ResourceName
mkExForm = newForm [
    (str ":" <+>) @@= editTextField id CmdLineFormRes (Just 1)
    ]

viewHex :: Int -> Int -> String
viewHex 0 _ = ""
viewHex n d = c !! shifted : viewHex (n-1) masked
    where c = "0123456789abcdef"
          s = 4*(n-1)
          shifted = (shiftR d s) .&. 0xf
          masked = d .&. ((shiftL 1 s)-1)

viewAscii :: Word8 -> Widget ResourceName
viewAscii w
    | w < 32 = str "."
    | otherwise = str [chr $ fromIntegral w]

interleave :: [Widget ResourceName] -> [Widget ResourceName]
interleave [] = []
interleave [w] = [w]
interleave (w:ws) = w : str " " : interleave ws

createRows :: Int -> Int -> Int -> [Word8] -> [Widget ResourceName]
createRows _ _ _ [] = []
createRows addrLength n i ws = str addr
              <+> str " "
              <+> (hBox . interleave) hexes
              <+> str "|"
              <+> hBox asciis
                : createRows addrLength n (i+n) (drop n ws) where
    bytes = (take n ws)
    hexes = fmap (str . viewHex 2 . fromIntegral) bytes
    asciis = fmap viewAscii bytes
    addr = viewHex addrLength i

viewEditor :: Model e -> Widget ResourceName
viewEditor m = Widget Greedy Greedy $ do
    let contents = fileContents m
    ctx <- getContext

    let addrLength = let len = (fromIntegral (BL.length contents) :: Double)
                     in ceiling $ logBase 16 len
    let bytesPerRow = div (ctx^.availWidthL - addrLength - 1) 4
    let byteCount = bytesPerRow * ctx^.availHeightL
    let bytes = take byteCount (drop (scrollPos m) (BL.unpack contents))
    render $ vBox (createRows addrLength bytesPerRow (scrollPos m) bytes)

viewCmdLine :: Model e -> Widget ResourceName
viewCmdLine m =
    case cmdMode m of
        CmdNone -> str $ filePath m ++ ": "
                      ++ show (cursorPos m) ++ ", "
                      ++ show (scrollPos m)
        CmdEx -> renderForm $ cmdForm m
        CmdSearch -> str " "

view :: Model e -> [Widget ResourceName]
view m =
    [ viewEditor m <=> viewCmdLine m ]

app :: App (Model e) e ResourceName
app = App { appDraw = view
          , appChooseCursor = showFirstCursor
          , appHandleEvent = update
          , appStartEvent = pure
          , appAttrMap = const $ attrMap mempty []
          }

main :: IO (Model e)
main = do
    args <- getArgs
    initial <- initialModel (args !! 0)
    defaultMain app initial
