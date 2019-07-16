module VHex.Window.ByteView
( ByteView
, byteView

-- * ByteView functions
, fromWord, toWord, displayWidth, spaceWidth

-- * ByteView implementations
, binary, oct, dec, hex
, ascii1, asciiC, asciiCtrl, asciiCaret
) where

import Numeric ( showHex, readHex
               , showOct, readOct
               , showInt, readDec
               , showIntAtBase, readInt
               )

import Data.Function (on)
import Data.Word (Word8)
import Data.Char (chr, ord, toUpper)

import VHex.Util (padIn, padOut, trim)

data BVName = Binary
            | Oct
            | Dec
            | Hex
            | Ascii1
            | AsciiCtrl
            | AsciiC
            | AsciiCaret
            deriving (Eq, Ord)

data ByteView = ByteView { _fromWord :: Word8 -> String
                         , _toWord :: String -> Maybe Word8
                         , _displayWidth :: Int
                         , _spaceWidth :: Int
                         , _bvName :: BVName
                         }

instance Eq ByteView where
    (==) = (==) `on` bvName
    (/=) = (/=) `on` bvName

instance Ord ByteView where
    (<)  = (>)  `on` bvName
    (<=) = (<=) `on` bvName
    (>)  = (>)  `on` bvName
    (>=) = (<=) `on` bvName

-- | Association list binding string names to implementations.
byteView :: [(String, ByteView)]
byteView = [ ("binary", binary)
           , ("oct", oct)
           , ("dec", dec)
           , ("hex", hex)
           , ("ascii1", ascii1)
           , ("asciictrl", asciiCtrl)
           , ("asciic", asciiC)
           , ("asciicaret", asciiCaret)
           ]

-- | Convert a single word to a string.
fromWord :: ByteView -> Word8 -> String
fromWord bv = padOut (displayWidth bv) ' ' . _fromWord bv

-- | Convert an input string to a word, if possible.
toWord :: ByteView -> String -> Maybe Word8
toWord bv = _toWord bv . trim

-- | Number of characters to display a single byte.
displayWidth :: ByteView -> Int
displayWidth = _displayWidth

-- | Width of space between bytes.
spaceWidth :: ByteView -> Int
spaceWidth = _spaceWidth

-- | Name associated with ByteView.
bvName :: ByteView -> BVName
bvName = _bvName

-- Converter helpers

fromWordShowS :: (Word8 -> ShowS) -> Word8 -> String
fromWordShowS f = padIn len '0' . (`f` "")
    where len = length $ f 0xff ""

toWordReadS :: ReadS Word8 -> String -> Maybe Word8
toWordReadS f str = case f str of
                        [(n,[])] -> Just n
                        _ -> Nothing

createByteView :: (Word8 -> ShowS) -> ReadS Word8 -> BVName -> ByteView
createByteView s r n = ByteView
    { _fromWord = fromWordShowS s
    , _toWord = toWordReadS r
    , _spaceWidth = sw
    , _displayWidth = dw
    , _bvName = n
    }
    where fw = fromWordShowS s
          dw = (length . fw) 0
          sw = if dw <= 1 then 0 else 1

-- ByteView implementations

-- | Binary representation, 8 bits per byte.
binary :: ByteView
binary = createByteView (showIntAtBase 2 intToDigit)
                        (readInt 2 isDigit valDigit)
                        Binary
    where isDigit '0' = True
          isDigit '1' = True
          isDigit _ = False
          valDigit '1' = 1
          valDigit _ = 0
          intToDigit 1 = '1'
          intToDigit _ = '0'

-- | Octal representation, zero-padded to two 3 characters.
oct :: ByteView
oct = createByteView showOct readOct Oct

-- | Decimal representation, space-padded to two 3 characters.
dec :: ByteView
dec = createByteView showInt readDec Dec

-- | Hexadecimal representation, zero-padded to two characters.
hex :: ByteView
hex = createByteView showHex readHex Hex

-- | Ascii representation with 1 character per byte. If byte is an character,
-- it is displayed, otherwise it is displayed as a dot.
ascii1 :: ByteView
ascii1 = ByteView
    { _fromWord = \w -> if w < 0x20 || w >= 0x7f
                        then "."
                        else (return . chr . fromIntegral) w
    , _toWord = \w -> case w of
                        "" -> Just 0
                        c:_ -> (Just . fromIntegral . ord ) c
    , _displayWidth = 1
    , _spaceWidth = 0
    , _bvName = Ascii1
    }

-- | If the byte is a character it is displayed and space padded to 3 bytes.
-- Escape sequences from C-standard are displayed like:
--  * NULL              -> \0 
--  * alert             -> \a
--  * backspace         -> \b
--  * form feed         -> \f
--  * newline           -> \n
--  * carriage return   -> \r
--  * tab               -> \t
--  * vertical tab      -> \v
-- Remaining bytes are displayed in octal.
asciiC :: ByteView
asciiC = ByteView
    { _fromWord = \w -> case w of
        0x00 -> "\\0"; 0x07 -> "\\a"; 0x08 -> "\\b"; 0x09 -> "\\t"
        0x0a -> "\\n"; 0x0b -> "\\v"; 0x0c -> "\\f"; 0x0d -> "\\r"
        _ | w < 0x20 || w >= 0x7f -> fromWordShowS showOct w
          | otherwise -> return $ chr $ fromIntegral w
    , _toWord  = \w -> case w of
        "\\0" -> Just 0x00; "\\a" -> Just 0x07; "\\b" -> Just 0x08
        "\\t" -> Just 0x09; "\\n" -> Just 0x0a; "\\v" -> Just 0x0b
        "\\f" -> Just 0x0c; "\\r" -> Just 0x0d
        [c] -> (Just . fromIntegral . ord) c
        str -> toWordReadS readOct str
    , _displayWidth = 3
    , _spaceWidth = 1
    , _bvName = AsciiC
    }

-- | Ascii characters with control codes represented by short names
-- Remaining bytes are displayed in hexadecimal.
asciiCtrl :: ByteView
asciiCtrl = ByteView
    { _fromWord = \w -> case w of
        0x00 -> "nul"; 0x01 -> "soh"; 0x02 -> "stx"; 0x03 -> "etx"
        0x04 -> "eot"; 0x05 -> "enq"; 0x06 -> "ack"; 0x07 -> "bel"
        0x08 -> "bs "; 0x09 -> "ht "; 0x0a -> "lf "; 0x0b -> "vt "
        0x0c -> "ff "; 0x0d -> "cr "; 0x0e -> "so "; 0x0f -> "si "
        0x10 -> "dle"; 0x11 -> "dc1"; 0x12 -> "dc2"; 0x13 -> "dc3"
        0x14 -> "dc4"; 0x15 -> "nak"; 0x16 -> "syn"; 0x17 -> "etb"
        0x18 -> "can"; 0x19 -> "em "; 0x1a -> "sub"; 0x1b -> "esc"
        0x1c -> "fs "; 0x1d -> "gs "; 0x1e -> "rs "; 0x1f -> "us "
        0x7f -> "del"
        _ | w >= 0x80 -> fromWordShowS showHex w
          | otherwise -> chr (fromIntegral w) : "  "
    , _toWord  = \w -> case w of
        "nul" -> Just 0x00; "soh" -> Just 0x01; "stx" -> Just 0x02
        "etx" -> Just 0x03; "eot" -> Just 0x04; "enq" -> Just 0x05
        "ack" -> Just 0x06; "bel" -> Just 0x07; "bs"  -> Just 0x08
        "ht"  -> Just 0x09; "lf"  -> Just 0x0a; "vt"  -> Just 0x0b
        "ff"  -> Just 0x0c; "cr"  -> Just 0x0d; "so"  -> Just 0x0e
        "si"  -> Just 0x0f; "dle" -> Just 0x10; "dc1" -> Just 0x11
        "dc2" -> Just 0x12; "dc3" -> Just 0x13; "dc4" -> Just 0x14
        "nak" -> Just 0x15; "syn" -> Just 0x16; "etb" -> Just 0x17
        "can" -> Just 0x18; "em " -> Just 0x19; "sub" -> Just 0x1a
        "esc" -> Just 0x1b; "fs"  -> Just 0x1c; "gs"  -> Just 0x1d
        "rs"  -> Just 0x1e; "us"  -> Just 0x1f; "del" -> Just 0x7f
        [c] -> (Just . fromIntegral . ord) c
        str -> toWordReadS readHex str
    , _displayWidth = 3
    , _spaceWidth = 1
    , _bvName = AsciiCtrl
    }

-- | Ascii characters with control characters displayed in caret notation.
-- Remaining bytes are displayed in hexadecimal.
asciiCaret :: ByteView
asciiCaret = ByteView
    { _fromWord = \w -> case w of
        _ | w < 0x20 || w == 0x7f -> ['^', chr $ fromIntegral $ toggleCaret w]
          | w >= 0x7f -> fromWordShowS showHex w
          | otherwise -> return $ chr $ fromIntegral w
    , _toWord = \w -> case w of
        ['^', c] -> (Just . fromIntegral . toggleCaret . ord . toUpper) c
        [c] -> (Just . fromIntegral . ord) c
        str -> toWordReadS readHex str
    , _displayWidth = 2
    , _spaceWidth = 1
    , _bvName = AsciiCaret
    } where toggleCaret w = mod (w+64) 128
