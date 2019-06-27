module VHex.Buffer
( Buffer
, empty, singleton, buffer
, selected, location, length
, slice, contents, null
, move, moveTo
, insert, replace, remove
) where

import Prelude hiding (length, null)

import Data.Word
import Data.Maybe (isNothing)
import Data.List (intersperse)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

data Buffer = Buffer { prev :: ByteString
                     , selected :: Maybe Word8
                     , location :: Int
                     , length :: Int
                     , next :: ByteString
                     }

instance Show Buffer where
    show buf = fromBS (prev buf) ++ sel ++ fromBS (next buf) where
        fromBS bs = intersperse ' ' $ concatMap show $ B.unpack bs
        sel = " [" ++ maybe " " show (selected buf) ++ "] "

-- constructing

empty :: Buffer
empty = Buffer B.empty Nothing 0 0 B.empty

singleton :: Word8 -> Buffer
singleton w = Buffer B.empty (Just w) 0 1 B.empty

buffer :: ByteString -> Buffer
buffer bs
    | B.null bs = empty
    | otherwise = let (h, t) = (B.head bs, B.tail bs)
                      len = fromIntegral $ B.length bs -- whole file read here
                  in Buffer B.empty (Just h) 0 len t

-- accessing / querying

slice :: Int -> Int -> Buffer -> ByteString
slice start count buf = case selected moved of
    Nothing -> B.empty
    Just v -> B.cons v $ B.take (fromIntegral count-1) (next moved)
    where moved = moveTo start buf

contents :: Buffer -> ByteString
contents buf = slice 0 (length buf - 1) buf

null :: Buffer -> Bool
null buf = length buf == 0

-- cursor modification

move :: Int -> Buffer -> Buffer
move d buf
    | fromIntegral d < -B.length (prev buf) =
        error "move: exceeding lower bounds:"
    | fromIntegral d > B.length (next buf) + 1 =
        error "move: exceeding upper bounds"
    | d < 0 = buf
        { prev = B.drop n (prev buf)
        , selected = Just $ B.index (prev buf) (n-1)
        , location = i+d
        , next = let beg = B.reverse (B.take (n-1) (prev buf))
                 in beg `B.append` sel `B.append` next buf
        }
    | d > 0 = buf
        { prev = let beg = B.reverse (B.take (n-1) (next buf))
                 in beg `B.append` sel `B.append` prev buf
        , selected = if (i+d) == length buf
                        then Nothing
                        else Just (B.index (next buf) (n-1))
        , location = i+d
        , next = B.drop n (next buf)
        }
    | otherwise = buf
    where
        sel = case selected buf of
                Nothing -> B.empty
                Just w -> B.singleton w
        i = location buf
        n = fromIntegral $ abs d

moveTo :: Int -> Buffer -> Buffer
moveTo i buf = move (i-location buf) buf

-- modifiation of contents

insert :: Word8 -> Buffer -> Buffer
insert w buf = case selected buf of
    Nothing -> if location buf == length buf
                then buf { selected = Just w
                         , location = length buf
                         , length = length buf + 1
                         }
                else error "insert out of bounds"
    Just v -> buf { selected = Just w
                  , length = length buf + 1
                  , next = B.cons v (next buf)
                  }

replace :: Word8 -> Buffer -> Buffer
replace w buf
    | null buf = insert w buf
    | isNothing (selected buf) = insert w buf
    | otherwise = buf { selected = Just w }

remove :: Buffer -> Buffer
remove buf
    | isNothing (selected buf) = buf
    | not $ B.null (next buf) = buf { selected = Just $ B.head (next buf)
                                    , length = length buf - 1
                                    , next = B.tail (next buf)
                                    }
    | not $ B.null (prev buf) = buf { selected = Nothing
                                    , length = length buf - 1
                                    }
    | not $ null buf = empty
    | otherwise = error "empty buffer"
