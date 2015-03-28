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
--
-- >>> max (Cell 1 1) (Cell 2 3)
-- Cell 2 3
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
-- | Show
-- >>> let acs = AliveCells [Cell 0 0]
-- >>> print acs
-- AliveCells [Cell 0 0]

data AliveCells = AliveCells [Cell] deriving (Show, Eq, Ord)

-- 世界がある
--
-- | Show World
-- >>> let world = AliveCells [Cell 0 0]
-- >>> print world
-- AliveCells [Cell 0 0]
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
--
-- | 次の世代の生きているCellを返す
-- >>> let world = AliveCells [Cell 0 0]
-- >>> nextGeneration world
-- AliveCells [Cell 0 0]
--
instance World AliveCells where
  alive (AliveCells cs) p = p `elem` cs
  nextGeneration w = w

-- | 世界を表示する
-- >>> let world = AliveCells [Cell 0 0]
-- >>> display [0..2] [0..2] world
-- o__
-- ___
-- ___
--
display :: World a => [Int] -> [Int] -> a -> IO()
display xs ys w =
  mapM_ (putStrLn . lineAt)  ys
    where
      lineAt y = map cellToChar [Cell a y| a <- xs]
      cellToChar c
        | alive w c = 'o'
        | otherwise = '_'

