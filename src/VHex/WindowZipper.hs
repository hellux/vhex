module VHex.WindowZipper
( WindowZipper
, empty, singleton
, modify
, split
, left, right, up, down
) where

data Axis = Horizontal | Vertical deriving (Eq, Show)
data Child w = ChildSplit [Child w]
             | ChildLeaf w
             deriving (Show)
data Parent w = Parent [Child w] -- previous children
                       [Child w] -- next children
             deriving (Show)
data WindowZipper w = EmptyZ
                    | SplitZ Axis [Parent w] [Child w]
                    | LeafZ  Axis [Parent w] w
                    deriving (Show)

empty :: WindowZipper w
empty = EmptyZ

singleton :: w -> WindowZipper w
singleton = LeafZ Horizontal []

modify :: (w -> w) -> WindowZipper w -> WindowZipper w
modify f (LeafZ ax above w) = LeafZ ax above (f w)
modify _ wz = wz

split :: Axis -> WindowZipper w -> WindowZipper w
split axis (LeafZ _ [] w) =
    LeafZ axis [Parent [] [ChildLeaf w]] w
split axis (LeafZ ax (Parent prev next:ss) w)
    | axis == ax = LeafZ axis ( Parent prev (ChildLeaf w : next)
                              : ss
                              ) w
    | otherwise  = LeafZ axis ( Parent [] [ChildLeaf w]
                              : Parent prev next
                              : ss
                              ) w
split _ tz = tz

nx -> Axis -> Axis
nx Horizontal = Vertical
nx Vertical = Horizontal

selectPrev :: Axis -> WindowZipper w -> WindowZipper w
selectPrev axis wz@(LeafZ ax (Parent prev@(sp:sps) next:ss) w)
    | ax == axis = case prev of
        (ChildLeaf v:ps) -> LeafZ axis newParents v
        (ChildSplit cs:ps) -> selectPrev axis $ SplitZ (nx axis) newParents cs
        [] -> case sp of -- TODO fix
            ChildLeaf v -> Leaf (nx axis) newParents v
            ChildSplit cs -> selectPrev $ SplitZ (nx axis) newParents cs
        _ -> wz
    where newParents = Parent sps (ChildLeaf w:next):ss
selectPrev _ wz = wz

selectNext :: Axis -> WindowZipper w -> WindowZipper w
selectNext axis wz@(LeafZ ax (Parent prev next:ss) w)
    | ax == axis = case next of
        (ChildLeaf v:ns) -> LeafZ axis (Parent (ChildLeaf w:prev) ns:ss) v
        _ -> wz
selectNext _ wz = wz

left :: WindowZipper w -> WindowZipper w
left = selectPrev Horizontal

right :: WindowZipper w -> WindowZipper w
right = selectNext Horizontal

up :: WindowZipper w -> WindowZipper w
up = selectPrev Vertical

down :: WindowZipper w -> WindowZipper w
down = selectNext Horizontal
