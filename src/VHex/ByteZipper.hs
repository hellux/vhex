{-|
Module      : VHex.Main
Description : Zipper for bytes in buffer.
Copyright   : (c) Noah Hellman, 2019
License     : GPL-3
Maintainer  : noah.hellman@protonmail.com
Stability   : unstable
Portability : not portable
-}

module VHex.ByteZipper
( ByteZipper
, empty, singleton, byteZipper
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

import Brick.Util (clamp)

data ByteZipper = ByteZipper
    { prev :: ByteString
    , selected :: Maybe Word8
    , location :: Int
    , length :: Int
    , next :: ByteString
    }

instance Show ByteZipper where
    show bz = fromBS (prev bz) ++ sel ++ fromBS (next bz)
        where fromBS bs = intersperse ' ' $ concatMap show $ B.unpack bs
              sel = " [" ++ maybe " " show (selected bz) ++ "] "

-- constructing

empty :: ByteZipper
empty = ByteZipper B.empty Nothing 0 0 B.empty

singleton :: Word8 -> ByteZipper
singleton w = ByteZipper B.empty (Just w) 0 1 B.empty

byteZipper :: ByteString -> ByteZipper
byteZipper bs
    | B.null bs = empty
    | otherwise = let (h, t) = (B.head bs, B.tail bs)
                      len = fromIntegral $ B.length bs -- whole file read here
                  in ByteZipper B.empty (Just h) 0 len t

-- accessing / querying

slice :: Int -> Int -> ByteZipper -> ByteString
slice start count bz = case selected moved of
    Nothing -> B.empty
    Just v -> B.cons v $ B.take (fromIntegral count-1) (next moved)
    where moved = moveTo start bz

contents :: ByteZipper -> ByteString
contents bz = slice 0 (length bz) bz

null :: ByteZipper -> Bool
null bz = length bz == 0

-- cursor modification

move :: Int -> ByteZipper -> ByteZipper
move d bz
    | d' < 0 = bz
        { prev = B.drop n (prev bz)
        , selected = Just $ B.index (prev bz) (n-1)
        , location = i+d'
        , next = let beg = B.reverse (B.take (n-1) (prev bz))
                 in beg `B.append` sel `B.append` next bz
        }
    | d' > 0 = bz
        { prev = let beg = B.reverse (B.take (n-1) (next bz))
                 in beg `B.append` sel `B.append` prev bz
        , selected = if (i+d') == length bz
                        then Nothing
                        else Just (B.index (next bz) (n-1))
        , location = i+d'
        , next = B.drop n (next bz)
        }
    | otherwise = bz
    where
        sel = case selected bz of
                Nothing -> B.empty
                Just w -> B.singleton w
        i = location bz
        d' :: Int
        d' = clamp (fromIntegral $ -B.length (prev bz))
                   (fromIntegral $  B.length (next bz) + 1)
                   d
        n = fromIntegral $ abs d'

moveTo :: Int -> ByteZipper -> ByteZipper
moveTo i bz = move (i-location bz) bz

-- modifiation of contents

insert :: Word8 -> ByteZipper -> ByteZipper
insert w bz = case selected bz of
    Nothing -> if location bz == length bz
                then bz { selected = Just w
                        , location = length bz
                        , length = length bz + 1
                        }
                else error "insert out of bounds, corrupted bz?"
    Just v -> bz { selected = Just w
                 , length = length bz + 1
                 , next = B.cons v (next bz)
                 }

replace :: Word8 -> ByteZipper -> ByteZipper
replace w bz
    | null bz = insert w bz
    | isNothing (selected bz) = insert w bz
    | otherwise = bz { selected = Just w }

remove :: ByteZipper -> ByteZipper
remove bz
    | isNothing (selected bz) = bz
    | not $ B.null (next bz) = bz { selected = Just $ B.head (next bz)
                                  , length = length bz - 1
                                  , next = B.tail (next bz)
                                  }
    | not $ B.null (prev bz) = bz { selected = Nothing
                                  , length = length bz - 1
                                  }
    | otherwise = empty
