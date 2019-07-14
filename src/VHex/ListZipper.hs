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
, beginning, end
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

instance Show (ListZipper Char) where
    show (ListZipper bef (sel:aft)) = bef ++ "[" ++ [sel] ++  "]" ++ aft
    show (ListZipper bef []) = bef ++ "[]"

empty :: ListZipper a
empty = ListZipper [] []

singleton :: a -> ListZipper a
singleton a = ListZipper [] [a]

fromList :: [a] -> ListZipper a
fromList = ListZipper []

toList :: ListZipper a -> [a]
toList (ListZipper bef aft) = reverse bef ++ aft

before :: ListZipper a -> [a]
before (ListZipper bef _) = reverse bef

selected :: ListZipper a -> Maybe a
selected (ListZipper _ []) = Nothing
selected (ListZipper _ (sel:_)) = Just sel

position :: ListZipper a -> Int
position (ListZipper bef _) = List.length bef

after :: ListZipper a -> [a]
after (ListZipper _ []) = []
after (ListZipper _ (_:aft)) = aft

null :: ListZipper a -> Bool
null (ListZipper [] []) = True
null _ = False

length :: ListZipper a -> Int
length = List.length . toList

take :: Int -> ListZipper a -> ListZipper a
take n (ListZipper bef aft)
    | n > len = ListZipper bef $ List.take (n-len) aft
    | n > 0 = let f:eb = reverse (List.take n bef) in ListZipper eb [f]
    | otherwise = ListZipper [] []
    where len = List.length bef

moveTo :: Int -> ListZipper a -> ListZipper a
moveTo pos lz@(ListZipper _ aft)
    | cur < pos = moveTo pos (right lz)
    | pos < cur = case aft of
                    [] -> lz
                    [_] -> lz
                    _ -> moveTo pos (left lz)
    | otherwise = lz
    where cur = position lz

move :: Direction -> ListZipper a -> ListZipper a
move Up = left
move Down = right

left :: ListZipper a -> ListZipper a
left lz@(ListZipper [] _) = lz
left (ListZipper (nextSel:bef) aft) = ListZipper bef (nextSel:aft)

right :: ListZipper a -> ListZipper a
right lz@(ListZipper _ []) = lz
right (ListZipper bef (sel:aft)) = ListZipper (sel:bef) aft

leftWrap :: ListZipper a -> ListZipper a
leftWrap (ListZipper [] aft) = let a:as = reverse aft in ListZipper as [a]
leftWrap lz = left lz

rightWrap :: ListZipper a -> ListZipper a
rightWrap (ListZipper bef [sel]) = ListZipper [] (reverse bef++[sel])
rightWrap lz = right lz

beginning :: ListZipper a -> ListZipper a
beginning lz@(ListZipper [] _) = lz
beginning lz = beginning (left lz)

end :: ListZipper a -> ListZipper a
end lz@(ListZipper _ []) = lz
end lz@(ListZipper _ [_]) = lz
end lz = end (right lz)

replace :: a -> ListZipper a -> ListZipper a
replace a (ListZipper bef (_:aft)) = ListZipper bef (a:aft)
replace a (ListZipper bef []) = ListZipper bef [a]

insert :: a -> ListZipper a -> ListZipper a
insert a (ListZipper bef aft) = ListZipper bef (a:aft)

remove :: ListZipper a -> ListZipper a
remove (ListZipper bef (_:aft)) = ListZipper bef aft
remove (ListZipper (_:bs) []) = ListZipper bs []
remove lz = lz

push :: a -> ListZipper a -> ListZipper a
push a (ListZipper bef aft) = ListZipper (a:bef) aft

pop :: ListZipper a -> ListZipper a
pop (ListZipper [] _) = empty
pop (ListZipper (_:bef) aft) = ListZipper bef aft
