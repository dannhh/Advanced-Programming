
-- get last elemnent in list
last1 xs = xs !! (length xs - 1)
last2 xs = head (reverse xs)

-- Remove last element in list
init1 xs = reverse (tail (reverse xs))
init2 :: [a] -> [a]
init2 [_] = []
init2 (x:xs) = x : init2 xs
