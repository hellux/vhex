module VHex.Types
( Model(..)
, Mode(..)
, Input(..), InsMode(..)
, CmdLine, CmdLineMode(..)
, Name(..)
, bvFocused, bufNull, bufLen, cursorPos, cursorVal
, moveTo, move, replace, insert, remove
) where


import Data.Word (Word8)
import Data.ByteString

import VHex.ByteZipper (ByteZipper)
import VHex.ListZipper (ListZipper)
import qualified VHex.ByteZipper as BZ
import VHex.ByteView (ByteView)

data Name = EditorViewPort
          | CmdLine
          | Cursor
          deriving (Show, Eq, Ord)

type CmdLine = ListZipper Char
data CmdLineMode = CmdNone
                    (Maybe
                        (Either
                            String   -- error message
                            String)) -- info message
                 | CmdEx
                    CmdLine
                    --(Maybe (ListZipper String)) -- tab complete suggestions
                 deriving (Show)

data Input = Input
                String  -- string representation of byte
                Int     -- cursor position on selected byte
            deriving (Show)
data InsMode = ReplaceMode
             | InsertMode
             deriving (Eq, Show)

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

bufNull :: Model -> Bool
bufNull = BZ.null . buffer

bufLen :: Model -> Int
bufLen = BZ.length . buffer

cursorPos :: Model -> Int
cursorPos = BZ.location . buffer

cursorVal :: Model -> Maybe Word8
cursorVal = BZ.selected . buffer

moveTo :: Int -> Model -> Model
moveTo i m = let i' = min (max 0 (bufLen m)) i
             in m { buffer = BZ.moveTo i' (buffer m) }

move :: Int -> Model -> Model
move n m = moveTo (cursorPos m+n) m

replace :: Word8 -> Model -> Model
replace w m = m { buffer = BZ.replace w (buffer m) }

insert :: Word8 -> Model -> Model
insert w m = m { buffer = BZ.insert w (buffer m) }

remove :: Model -> Model
remove m = if bufNull m
    then m
    else m { buffer = BZ.remove (buffer m) }
