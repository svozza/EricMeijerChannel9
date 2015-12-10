-- Ex01
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

-- Ex02
or' :: Bool -> Bool -> Bool
True  `or'` True  = True
True  `or'` False = True
False `or'` True  = True
False `or'` False = False

or'' :: Bool -> Bool -> Bool
False `or''` False = False
_  `or''` _ = True

or''' :: Bool -> Bool -> Bool
False  `or'''` b = b
True `or'''` _ = True

--Ex03
and' :: Bool -> Bool -> Bool
and' a b = if a then
              if b then True else False
           else False

--Ex04
and'' :: Bool -> Bool -> Bool
and'' a b = if a then b else False