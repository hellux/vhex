module VHex.WindowZipper
( WindowZipper
, empty, singleton
, modify
, split
) where

data Axis = None | Horizontal | Vertical deriving (Eq, Show)
data Child a = ChildSplit (Split a)
             | ChildLeaf a
             deriving (Show)
data Split a = Split [Child a] -- previous children
                     [Child a] -- next children
             deriving (Show)
data WindowZipper a = Empty
                    | Leaf 
                        Axis
                        [Split a] -- parent splits
                        a         -- window in leaf
                    deriving (Show)

empty :: WindowZipper a
empty = Empty

singleton :: a -> WindowZipper a
singleton = Leaf None []

modify :: (a -> a) -> WindowZipper a -> WindowZipper a
modify f (Leaf ax above a) = Leaf ax above (f a)
modify _ wz = wz

split :: Axis -> WindowZipper a -> WindowZipper a
split axis (Leaf _ [] a) =
    Leaf axis [Split [] [ChildLeaf a]] a
split axis (Leaf ax (Split prev next:ss) a)
    | axis == ax = Leaf axis ( Split prev (ChildLeaf a : next)
                             : ss
                             ) a
    | otherwise  = Leaf axis ( Split [] [ChildLeaf a]
                             : Split prev next
                             : ss
                             ) a
split _ tz = tz

left :: WindowZipper a -> WindowZipper a
left wz@(Leaf Horizontal (Split prev next:ss) a) = case prev of
    (ChildLeaf b:ps) -> Leaf Horizontal (Split ps (ChildLeaf a:next):ss) b
    _ -> wz
left wz = wz

right :: WindowZipper a -> WindowZipper a
right wz@(Leaf Horizontal (Split prev next:ss) a) = case next of
    (ChildLeaf b:ns) -> Leaf Horizontal (Split (ChildLeaf a:prev) ns:ss) b
    _ -> wz
right wz = wz
