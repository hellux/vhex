module VHex.ListZipper ( ListZipper
                       , empty, singleton
                       , fromList, toList
                       , before, selected, after
                       , position
                       , length
                       , left, right
                       , push, pop
                       ) where

import Prelude hiding (null, length)

import qualified Data.List as List

data ListZipper a = ListZipper [a] [a] deriving (Show)

instance Functor ListZipper where
    fmap f (ListZipper bef saft) = ListZipper (fmap f bef) (fmap f saft)

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

selected :: ListZipper a -> a
selected (ListZipper _ []) = error "empty ListZipper"
selected (ListZipper _ (sel:_)) = sel

position :: ListZipper a -> Int
position (ListZipper bef _) = List.length bef

after :: ListZipper a -> [a]
after (ListZipper _ []) = []
after (ListZipper _ (_:aft)) = aft

length :: ListZipper a -> Int
length = List.length . toList

left :: ListZipper a -> ListZipper a
left lz@(ListZipper [] _) = lz
left (ListZipper (nextSel:bef) aft) = ListZipper bef (nextSel:aft)

right :: ListZipper a -> ListZipper a
right lz@(ListZipper _ []) = lz
right (ListZipper bef (sel:aft)) = ListZipper (sel:bef) aft

push :: a -> ListZipper a -> ListZipper a
push a (ListZipper bef aft) = ListZipper (a:bef) aft

pop :: ListZipper a -> ListZipper a
pop (ListZipper [] _) = empty
pop (ListZipper (_:bef) aft) = ListZipper bef aft
