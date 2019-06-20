module VHex.Buffer
( Buffer
, empty, singleton, buffer
, selected, selectedValue, selectedIndex, length
, index, slice, contents, null
, move, moveTo
, insert, replace, remove
) where

import Prelude hiding (length, null)

import Data.Word

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

data Buffer = Buffer { prev :: ByteString
                     , selected :: Maybe (Word8, Int)
                     , length :: Int
                     , next :: ByteString
                     } deriving (Show)

-- constructing

empty :: Buffer
empty = Buffer B.empty Nothing 0 B.empty

singleton :: Word8 -> Buffer
singleton w = Buffer B.empty (Just (w, 0)) 1 B.empty

-- TODO use strict bytestring or get length from file system?
buffer :: ByteString -> Buffer
buffer bs
    | B.null bs = empty
    | otherwise = let (h, t) = (B.head bs, B.tail bs)
                      len = fromIntegral $ B.length bs -- whole file read here
                  in Buffer B.empty (Just (h, 0)) len t

-- accessing

selectedValue :: Buffer -> Maybe Word8
selectedValue buf = case selected buf of
    Nothing -> Nothing
    Just (v, _) -> Just v

selectedIndex :: Buffer -> Maybe Int
selectedIndex buf = case selected buf of
    Nothing -> Nothing
    Just (_, i) -> Just i

index :: Buffer -> Int -> Word8
index buf pos =
    case (selected buf) of
        Nothing -> error "empty buffer"
        Just sel -> index' sel
    where
        index' (v, i)
            | pos < i = B.index (prev buf) $ fromIntegral (i-pos-1)
            | pos > i = B.index (next buf) $ fromIntegral (pos-i-1)
            | otherwise = v

slice :: Int -> Int -> Buffer -> ByteString
slice start count buf = B.cons first $
                               B.take (fromIntegral count-1) (next moved)
    where moved = moveTo start buf
          Just (first, _) = selected moved

contents :: Buffer -> ByteString
contents buf = slice 0 (length buf - 1) buf

null :: Buffer -> Bool
null buf = case selected buf of
    Nothing -> True
    Just _ -> False

-- cursor modification

move :: Int -> Buffer -> Buffer
move d buf
    | d < 0 = buf
        { prev = B.drop n (prev buf)
        , selected = Just (B.index (prev buf) (n-1), i+d)
        , next = let beg = B.reverse (B.take (n-1) (prev buf))
                 in beg `B.append` B.cons s (next buf)
        }
    | d > 0 = buf
        { prev = let beg = B.reverse (B.take (n-1) (next buf))
                 in beg `B.append` B.cons s (prev buf)
        , selected = Just (B.index (next buf) (n-1), i+d)
        , next = B.drop n (next buf)
        }
    | otherwise = buf
    where
        Just (s, i) = selected buf
        n = fromIntegral $ abs d

moveTo :: Int -> Buffer -> Buffer
moveTo pos buf = case selected buf of
    Nothing -> error "empty buffer"
    Just (_, i) -> move (pos-i) buf

-- modifiation of contents

insert :: Word8 -> Buffer -> Buffer
insert w buf
    | null buf = singleton w
    | otherwise = buf { selected = Just (w, i)
                      , length = (length buf) + 1
                      , next = B.cons v (next buf)
                      }
    where Just (v, i) = selected buf

replace :: Word8 -> Buffer -> Buffer
replace w buf
    | null buf = error "empty buffer"
    | otherwise = buf { selected = Just (w, i) }
    where Just (_, i) = selected buf

remove :: Buffer -> Buffer
remove buf
    | not $ B.null (next buf) = buf { selected = Just (B.head (next buf), i)
                                    , length = (length buf) - 1
                                    , next = B.tail (next buf)
                                    }
    | not $ B.null (prev buf) = buf { prev = B.tail (prev buf)
                                    , selected = Just (B.head (prev buf), i-1)
                                    , length = (length buf) - 1
                                    }
    | not $ null buf = empty
    | otherwise = error "empty buffer"
    where Just (_, i) = selected buf
