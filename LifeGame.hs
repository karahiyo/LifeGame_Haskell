module LifeGame
where

-- | 碁盤のような格子があり、一つの格子はセル（細胞）と呼ばれる
-- >>> Cell 1 2
-- Cell 1 2
--
-- >>> (Cell 1 2) == (Cell 1 2)
-- True
--
-- >>> (Cell 1 2) == (Cell 0 2)
-- False
--
-- >>> (Cell 1 2) > (Cell 1 3)
-- False
data Cell = Cell Int Int deriving (Show, Eq, Ord)

-- | ムーア近傍
-- >>> neighbours (Cell 2 2)
-- [Cell 1 1,Cell 1 2,Cell 1 3,Cell 2 1,Cell 2 3,Cell 3 1,Cell 3 2,Cell 3 3]
neighbours :: Cell -> [Cell]
neighbours (Cell x y) =
  [Cell a b|
    a <- [x - 1, x, y + 1],
    b <- [y - 1, y, y + 1],
    (Cell x y) /= (Cell a b)
  ]

-- | 生きているCellのリスト
-- >>> AliveCells [Cell 0 0,  Cell 1 1,  Cell 2 3]
-- AliveCells [Cell 0 0,Cell 1 1,Cell 2 3]
--
data AliveCells = AliveCells [Cell] deriving (Show)

-- | 世界がある
class World a where
  alive :: a -> Cell -> Bool
  nextGeneration :: a -> a

-- | 世界は生きているCellを知っている(世界は無限なので死んでいるCellは見ない)
-- >>> let world = AliveCells [Cell 1 1, Cell 2 3]
-- >>> alive world (Cell 1 1)
-- True
--
-- >>> alive world (Cell 0 0)
-- False
instance World AliveCells where
  alive (AliveCells cs) p = p `elem` cs

