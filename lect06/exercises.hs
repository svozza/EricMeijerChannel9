-- Ex01 (a)
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- Ex01 (b)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- Ex01 (c)
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n - 1) x

-- Ex01 (d)
nth :: [a] -> Int -> a
nth (x:_) 0 = x
nth (_:xs) n = nth xs (n - 1)

-- Ex01 (e)
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) = e == x || elem' e xs

-- Ex02
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

-- Ex03
msort :: [Int] -> [Int]
msort xs | length xs <= 1 = xs
         | otherwise = merge (msort h1) (msort h2)
           where (h1, h2) = splitAt (div (length xs) 2) xs