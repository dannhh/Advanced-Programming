-- ======================= RECURSIVE FUNCTIONS ====================
import Prelude hiding (length, reverse, (++), zip, drop, odd, even, (^), and, concat, replicate, (!!), elem, merge)

-- ============================ BASICS ============================
-- length of list
length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + length xs

-- reverse a list
reverse :: [Int] -> [Int]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- (++) operator
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- insert value in sorted list
insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:ys) | y < x     = y : insert x ys
                | otherwise = x : y : ys
                
-- ======================== MULTIPLE ARGUMENTS ====================
-- redefine zip function
zip :: [a] -> [b] -> [(a, b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y): zip xs ys

-- redefine drop function
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = drop (n-1) xs

-- ======================== MULTIPLE RECURSION ====================
-- define fibonacci function
fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- define quicksort
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where smaller = [a | a <- xs, a <= x]
                     larger  = [b | b <- xs, b  > x]
                     
-- ======================= MUTUAL RECURSION ===================
-- even and odd Int
even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n-1)

-- even and odd list
evens :: [a] -> [a] 
evens []  = [] 
evens (x:xs) = x : odds xs

odds :: [a] -> [a] 
odds []  = [] 
odds (x:xs) = x : evens xs

-- =========================== EXERCISEs ==========================
-- calculate factorial
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac(n-1)
      | otherwise = 1
      
-- sumDown 3 = 3 + 2 + 1 + 0 = 6
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- exponentiation operator
(^) :: Int -> Int -> Int 
_ ^ 0 = 1
n ^ m = n * (n ^ (m - 1))

-- GCD: Euclid algo
euclid :: Int -> Int -> Int 
euclid 0 _ = 0
euclid _ 0 = 0
euclid a b | a > b = euclid b a
           | b `mod` a == 0 = a
           | otherwise = euclid (b - a) a
           
-- decide if all logical values in a list are True:
and :: [Bool] -> Bool
and [] = True
and (x:xs) | x == True = and xs
           | otherwise = False

-- concatenate a list of lists:
concat :: [[a]] -> [a]
concat [] = []
concat ([]:xss) = concat xss
concat ((x:xs):xss) = x : concat (xs:xss) 

-- produce a list with n identical elements:
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n a = a : replicate (n-1) a

-- select the nth element of a list:
(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

-- decide if a value is an element of a list:
elem :: Eq a => a -> [a] -> Bool
elem a [] = False
elem a (x:xs) | x == a = True
              | otherwise = elem a xs
-- merges two sortedlists to give a single sorted list.
merge :: Ord a => [a] -> [a] -> [a] 
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys 

-- merge sort, (empty and singleton lists are already sorted),
-- and any other list is sorted by merging together the two lists
-- that result from sorting the two halves of the list separately
halves :: Ord a => [a] -> ([a], [a])
halves xs = (take n xs, drop n xs)
            where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left)  (msort right)
           where (left, right) = halves xs

-- bonus question 9
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take _ [] = []
take 0 xs = [] 
take n (x:xs) = x : take (n - 1) xs

last :: [a] -> a
last [x] = x
last (x:xs) = last xs
