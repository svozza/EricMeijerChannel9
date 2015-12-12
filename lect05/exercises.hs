-- Ex01
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

--Ex02
factors  :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

--Ex03
scalar :: [Int] -> [Int] -> [Int]
scalar xs ys = [(\(x, y) -> x * y) zipped | zipped <- zip xs ys]