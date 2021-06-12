import Prelude hiding (concat, length, replicate) 

-- LIST COMPREHENSIONS 

-- =============== Basic concept ==============================
-- concatenates a list of lists can be defined by using one 
-- generator to select each list in turn, and another to select
-- each element from eachlist:
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- selects all the first components from a list of pair
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x, _) <- ps]

-- length of a list 
length :: [a] -> Int
length xs = sum [1 | _ <- xs]

-- =================== Guard ==================================
-- Guard is list comprehension use logical expressions.
-- maps a positiveinteger to its list of positive factors:
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- decides if an integer is prime
prime :: Int -> Bool
prime n = factors n == [1, n]

-- list of allprime numbers up to a given limit
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- returns the list of all values that are associated with 
-- a given key in a table
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']
-- > find ’b’ [(’a’,1),(’b’,2),(’c’,3),(’b’,4)]
-- [2,4]

-- ====================== ZIP function ========================
-- > zip [’a’,’b’,’c’] [1,2,3,4]
-- [(’a’,1),(’b’,2),(’c’,3)]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
-- > pairs [1,2,3,4]
-- [(1,2),(2,3),(3,4)]

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- returns the list of all positions at which a value occurs in alist
-- For ex: > positions False [True, False, True, False]
--         [1,3]
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

-- =================== String comprehensions ==================
-- function return the number of lower-case letters and 
-- particular characters that occur in a string, respectively
lowers :: String -> Int 
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count s xs = length [x | x <- xs, x == s]

-- ============================================================
-- ===================== EXERCISE =============================
-- ============================================================

-- sum of square 1..100
sumOfSquare = sum [x ^ 2 | x <- [1..100]]

-- print the list of all pairs (x, y) which are the 
-- coordinate grid of a given size 0 <= x <= m, 0 <= y <= n
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(a, b) | a <- [0..m], b <- [0..n]]

-- coordinate square of size n, excluding the diagonal 
-- from (0, 0) to (n,n).
square :: Int -> [(Int, Int)]
square m = [(a,b) | a <- [0..m], b <- [0..m], a /= b]

-- replicate :: Int -> a -> [a] by using a list comprehension
replicate :: Int -> a -> [a]
replicate m n = [n | _ <- [1..m]]

-- check Pythagorean (x2 + y2 = z2) using comprehension
pyths :: Int -> [(Int, Int, Int)]
pyths a = [(m, n, p) | m <- [1..10], n <- [1..10], p <- [1..10], m*m + n*n == p*p] 

-- a positive integer is perfect if it equals the sum of 
-- all of its factors, excluding the number itself.
perfects :: Int -> [Int]
perfects a = [x | x <- [1..a], (sum (factors x)) == 2*x]

-- [(x,y) | x <- [1,2], y <- [3,4]] from 2 to 1 generator
ex7 = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- redefine the function positions using the function find.
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [k | k <- find x (zip xs [0..])]

-- scalar product:  > scalarproduct [1,2,3] [4,5,6]
--                  32
scalarproduct :: [Int] -> [Int]-> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]
