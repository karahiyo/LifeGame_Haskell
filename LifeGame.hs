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
    a <- [x - 1, x, x + 1],
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
-- >>> alive world (Cell 0 (-1))
-- False
--
-- | 次の世代の生きているCellを返す
-- >>> let world = AliveCells [Cell 0 0]
-- >>> nextGeneration world
-- AliveCells []
--
-- >>> let w = AliveCells [Cell 0 0, Cell 1 0, Cell 0 1]
-- >>> nextGeneration w
-- AliveCells [Cell 0 0,Cell 1 0,Cell 0 1]
--
-- >>> :{
--  let w = AliveCells [Cell 0 0,  Cell 1 0, Cell 2 0,
--                      Cell 0 1,            Cell 2 1,
--                      Cell 0 2,  Cell 1 2, Cell 2 2]
--  in  nextGeneration w
-- :}
-- AliveCells [Cell 0 0,Cell 2 0,Cell 0 2,Cell 2 2]
--
instance World AliveCells where
  alive (AliveCells cs) p = p `elem` cs
  nextGeneration w@(AliveCells cs) = AliveCells nextAliveCells where
    notOverpopuration = filter (not . overpopuration w) cs
    notDepopuration = filter (not . depopuration w) notOverpopuration
    nextAliveCells = notDepopuration

-- | 生存 - 生きているセルに隣接する生きたセルが2つか3つならば、次の世代でも生存する。
survive :: World a => a -> Cell -> Bool
survive w c =
  let num = length $ filter (alive w) $ neighbours c
  in num == 2 || num == 3

-- | 過密 - 生きているセルに隣接する生きたセルが4つ以上ならば、過密により死滅する。
-- >>> :{
--  let w = AliveCells [Cell 0 0,  Cell 1 0, Cell 2 0,
--                      Cell 0 1,            Cell 2 1,
--                      Cell 0 2,  Cell 1 2, Cell 2 2]
-- :}
--
-- >>> overpopuration w (Cell 1 1)
-- True
-- >>> overpopuration w (Cell 2 1)
-- True
-- >>> overpopuration w (Cell 1 3)
-- False
--
overpopuration :: World a => a -> Cell -> Bool
overpopuration w c =
  let num = length $ filter (alive w) $ neighbours c
  in num >= 4

-- | 過疎 - 生きているセルに隣接する生きたセルが1つ以下ならば、過疎により死滅する。
-- >>> let w = AliveCells [Cell 0 0, Cell 0 1, Cell 1 0]
-- >>> depopuration w (Cell 0 0)
-- False
-- >>> depopuration w (Cell 1 0)
-- False
-- >>> depopuration w (Cell 0 1)
-- False
-- >>> depopuration w (Cell 0 99)
-- True
--
depopuration :: World a => a -> Cell -> Bool
depopuration w c =
  let num = length $ filter (alive w)  $ neighbours c
  in num <= 1

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

