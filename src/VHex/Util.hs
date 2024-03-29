{-|
Module      : VHex.Main
Description : Various utility functions.
Copyright   : (c) Noah Hellman, 2019
License     : GPL-3
Maintainer  : noah.hellman@protonmail.com
Stability   : unstable
Portability : not portable
-}

module VHex.Util ( hexLength
                 , floorN
                 , padIn, padOut
                 , groupsOf
                 , fromDir
                 , trim
                 , clamp
                 ) where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)

import Brick.Types (Direction(Up, Down))
import Brick.Util (clamp)

fromDir :: Direction -> Int
fromDir Up = -1
fromDir Down = 1

-- | Character length of hex representation of number.
hexLength :: Int -> Int
hexLength = max 1 . ceiling . logBase 16 . (fromIntegral :: Int -> Double)

-- | Round down to closest multiple of n.
floorN :: Int -> Int -> Int
floorN n x = x - mod x n

-- | Prepend xs with p until length is n.
padIn :: Int -> a -> [a] -> [a]
padIn n p xs
    | len < n = replicate (n-len) p ++ xs
    | otherwise = xs
    where len = length xs


-- | Append xs with p until length is n.
padOut :: Int -> a -> [a] -> [a]
padOut n p xs
    | len < n = xs ++ replicate (n-len) p
    | otherwise = xs
    where len = length xs

-- | Split xs into lists with length n.
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = let (y,ys) = splitAt n xs
                in y : groupsOf n ys

trim :: String -> String
trim " " = " "
trim s = (dropWhileEnd isSpace . dropWhile isSpace) s
