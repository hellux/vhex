{-# LANGUAGE TemplateHaskell #-}

import System.IO (FilePath)
import System.Environment (getArgs)

import qualified Data.Text as T

import Data.Bits
import Data.Word
import Data.Char (chr)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Graphics.Vty.Input.Events

import Lens.Micro.TH
import Lens.Micro

import Brick.Main
import Brick.AttrMap
import Brick.Forms
import Brick.Types
import Brick.Widgets.Core

data ResourceName = EditorViewPort
                  | CmdLineFormRes
                  | EditorBuffer
                  deriving (Show, Eq, Ord)

data CmdLineForm = CmdLineForm
    { _ex :: T.Text
    } deriving (Show)
data CmdLineMode = CmdNone
                 | CmdEx
                 | CmdSearch
                 deriving (Show)
data CmdState e = CmdState
    { _cmdMode :: CmdLineMode
    , _cmdForm :: Form CmdLineForm e ResourceName
    }

data ViewportState = ViewportState
    { _cursorPos :: Int
    , _scrollPos :: Int
    } deriving (Show)

data FileState = FileState
    { _filePath :: FilePath
    , _fileContents :: ByteString
    } deriving (Show)

data Model e = Model
    { _file :: FileState
    , _cmd :: (CmdState e)
    , _vp :: ViewportState
    }

makeLenses ''CmdLineForm
makeLenses ''CmdLineMode
makeLenses ''CmdState
makeLenses ''ViewportState
makeLenses ''FileState
makeLenses ''Model

initialModel :: FilePath -> IO (Model e)
initialModel fname = do
    contents <- BL.readFile fname
    return $ Model
        { _file = FileState
            { _filePath = fname
            , _fileContents = contents
            }
        , _cmd = CmdState
            { _cmdMode = CmdNone
            , _cmdForm = (mkExForm $ CmdLineForm T.empty)
            }
        , _vp = ViewportState
            { _cursorPos = 0
            , _scrollPos = 0
            }
        }

-- TODO change pos by n rows
scroll :: Int -> Model e -> Model e
scroll n m = m & vp.scrollPos %~ incr where
    incr prev = min (max 0 (n+prev)) $ fromIntegral (BL.length (m^.file.fileContents)-1)

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
        EvKey KEsc [] -> continue $ m & cmd.cmdMode .~ CmdNone
        EvKey KEnter [] -> continue $ m & cmd.cmdMode .~ CmdNone
        _ -> do
            cmdForm' <- handleFormEvent (VtyEvent vtye) (m^.cmd.cmdForm)
            continue (m & cmd.cmdForm .~ cmdForm')

update :: Model e -> BrickEvent ResourceName e
       -> EventM ResourceName (Next (Model e))
update m e =
    case e of
        VtyEvent vtye ->
            case m^.cmd.cmdMode of
                CmdNone ->
                    case vtye of
                        EvKey (KChar ':') [] ->
                            continue (m & cmd.cmdMode .~ CmdEx)
                        _ ->
                            normalMode m vtye
                CmdEx -> exCmd m vtye
                CmdSearch -> continue m
        _ -> continue m

mkExForm :: CmdLineForm -> Form CmdLineForm e ResourceName
mkExForm = newForm [
    (str ":" <+>) @@= editTextField ex CmdLineFormRes (Just 1)
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
    let contents = m^.file.fileContents
    ctx <- getContext

    let addrLength = let len = (fromIntegral (BL.length contents) :: Double)
                     in ceiling $ logBase 16 len
    let bytesPerRow = div (ctx^.availWidthL - addrLength - 1) 4
    let byteCount = bytesPerRow * ctx^.availHeightL
    let bytes = take byteCount (drop (m^.vp.scrollPos) (BL.unpack contents))
    render $ vBox (createRows addrLength bytesPerRow (m^.vp.scrollPos) bytes)

viewCmdLine :: Model e -> Widget ResourceName
viewCmdLine m =
    case m^.cmd.cmdMode of
        CmdNone -> str $ m^.file.filePath ++ ": "
                      ++ show (m^.vp.cursorPos) ++ ", "
                      ++ show (m^.vp.scrollPos)
        CmdEx -> renderForm $ m^.cmd.cmdForm
        CmdSearch -> str " "

view :: Model e -> [Widget ResourceName]
view m =
    [ viewEditor m <=>
      viewCmdLine m ]

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
