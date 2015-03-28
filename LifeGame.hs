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

