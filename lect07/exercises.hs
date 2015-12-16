filteredMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filteredMap pred f xs = map f (filter pred xs)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x xs' -> (f x):xs') [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred xs = foldr (\x xs' -> if pred x then x:xs'
                                   else xs') [] xs