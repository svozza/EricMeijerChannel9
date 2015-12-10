last' xs = xs !! (length xs - 1)

last'' xs =  drop (length xs - 1) xs !! 0

init' xs = take (length xs - 1) xs

init'' xs = reverse (drop 1 (reverse xs))
