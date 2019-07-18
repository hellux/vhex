{-|
Module      : VHex.Main
Description : General list zipper.
Copyright   : (c) Noah Hellman, 2019
License     : GPL-3
Maintainer  : noah.hellman@protonmail.com
Stability   : unstable
Portability : not portable
-}

{-# LANGUAGE FlexibleInstances #-}
module VHex.ListZipper
( ListZipper

-- * Construction
, empty, singleton
, fromList

-- * Query
, null
, length
, selected
, position
, before, after

-- * List manipulation
, take
, toList

-- * Cursor operations
, move, moveTo
, left, right
, leftWrap, rightWrap
, beginning, end, edge
, replace
, insert, remove
, push, pop
) where

import Prelude hiding (null, length, take)

import qualified Data.List as List
import Brick.Types (Direction(Up, Down))

data ListZipper a = ListZipper [a] [a]

instance Functor ListZipper where
    fmap f (ListZipper bef saft) = ListZipper (fmap f bef) (fmap f saft)

instance Show a => Show (ListZipper a) where
    show (ListZipper bef (sel:aft)) = show bef ++ " ["
                                   ++ show sel ++  "] "
                                   ++ show aft
    show (ListZipper bef []) = show bef ++ " []"

-- | Create an empty ListZipper.
empty :: ListZipper a
empty = ListZipper [] []

-- | Create a ListZipper with a single element.
singleton :: a -> ListZipper a
singleton a = ListZipper [] [a]

-- | Create a ListZipper from a list.
fromList :: [a] -> ListZipper a
fromList = ListZipper []

-- | Convert a ListZipper to a list.
toList :: ListZipper a -> [a]
toList (ListZipper bef aft) = reverse bef ++ aft

-- | Return the elements that appear before the cursor in the ListZipper.
before :: ListZipper a -> [a]
before (ListZipper bef _) = reverse bef

-- | Return the selected element. Is nothing if the ListZipper is empty or if
-- the selection is right after the last element.
selected :: ListZipper a -> Maybe a
selected (ListZipper _ []) = Nothing
selected (ListZipper _ (sel:_)) = Just sel

-- | Return the elements that appear after the cursor in the ListZipper.
after :: ListZipper a -> [a]
after (ListZipper _ []) = []
after (ListZipper _ (_:aft)) = aft

-- | 0-indexed position of the cursor in the ListZipper.
position :: ListZipper a -> Int
position (ListZipper bef _) = List.length bef

-- | Test whether the ListZipper is empty.
null :: ListZipper a -> Bool
null (ListZipper [] []) = True
null _ = False

-- | Return the number of elements in the ListZipper.
length :: ListZipper a -> Int
length = List.length . toList

-- | Return the prefix of length n of the ListZipper, starting from the
-- beginning of the list.
take :: Int -> ListZipper a -> ListZipper a
take n (ListZipper bef aft)
    | n > len = ListZipper bef $ List.take (n-len) aft
    | n > 0 = let f:eb = reverse (List.take n bef) in ListZipper eb [f]
    | otherwise = ListZipper [] []
    where len = List.length bef

-- | Move the cursor to a zero-indexed position.
moveTo :: Int -> ListZipper a -> ListZipper a
moveTo pos lz@(ListZipper _ aft)
    | cur < pos = moveTo pos (right lz)
    | pos < cur = case aft of
                    [] -> lz
                    [_] -> lz
                    _ -> moveTo pos (left lz)
    | otherwise = lz
    where cur = position lz

-- | Move the cursor one step in either direction.
move :: Direction -> ListZipper a -> ListZipper a
move Up = left
move Down = right

-- | Move the cursor one step to the left.
left :: ListZipper a -> ListZipper a
left lz@(ListZipper [] _) = lz
left (ListZipper (nextSel:bef) aft) = ListZipper bef (nextSel:aft)

-- | Move the cursor one step to the right, unless the cursor is currently
-- after the last element .
right :: ListZipper a -> ListZipper a
right lz@(ListZipper _ []) = lz
right (ListZipper bef (sel:aft)) = ListZipper (sel:bef) aft

-- | Move the cursor one step to the right, wrap around to the last element if
-- the cursor is on the first.
leftWrap :: ListZipper a -> ListZipper a
leftWrap (ListZipper [] aft) = let a:as = reverse aft in ListZipper as [a]
leftWrap lz = left lz

-- | Move the cursor one step to the right, wrap around to the first element if
-- the cursor is on the last.
rightWrap :: ListZipper a -> ListZipper a
rightWrap (ListZipper bef [sel]) = ListZipper [] (reverse bef++[sel])
rightWrap lz = right lz

-- | Move the cursor to the first element.
beginning :: ListZipper a -> ListZipper a
beginning lz@(ListZipper [] _) = lz
beginning lz = beginning (left lz)

-- | Move the cursor to the last element.
end :: ListZipper a -> ListZipper a
end lz@(ListZipper _ []) = lz
end lz@(ListZipper _ [_]) = lz
end lz = end (right lz)

-- | Move the cursor to the spot after the last element.
edge :: ListZipper a -> ListZipper a
edge lz@(ListZipper _ []) = lz
edge lz = edge (right lz)

-- | Replace the element at the cursor.
replace :: a -> ListZipper a -> ListZipper a
replace a (ListZipper bef (_:aft)) = ListZipper bef (a:aft)
replace a (ListZipper bef []) = ListZipper bef [a]

-- | Insert an element before the selected element and select the new element.
insert :: a -> ListZipper a -> ListZipper a
insert a (ListZipper bef aft) = ListZipper bef (a:aft)

-- | Remove the selected element and select the next elment.
remove :: ListZipper a -> ListZipper a
remove (ListZipper bef (_:aft)) = ListZipper bef aft
remove lz = lz

-- | Insert an element before the selected element and keep the selection on
-- the current element.
push :: a -> ListZipper a -> ListZipper a
push a (ListZipper bef aft) = ListZipper (a:bef) aft

-- | Remove the element before the cursor and keep the selection on the current
-- element.
pop :: ListZipper a -> ListZipper a
pop (ListZipper [] _) = empty
pop (ListZipper (_:bef) aft) = ListZipper bef aft
