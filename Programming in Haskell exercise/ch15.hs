-- ===================== LAZY EVALUATION ============================
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

sumwidth :: Int -> [Int] -> Int 
sumwidth v [] = v
sumwidth v (x:xs) = sumwidth (v+x) xs

-- ======================== EXERCISES ===============================

-- 4. Using a list comprehension, define an expression fibs :: [Integer]
-- that generates the infinitesequence of Fibonacci numbers
fibs :: [Integer]
fibs = 0 : 1 : [f0 + f1 | (f0, f1) <- zip fibs (tail fibs)]

-- 5. Define appropriate versions of the library functions for the 
-- following type of binary trees:
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show
              
repeatTree :: a -> Tree a
repeatTree x = Node t x t
               where t = repeatTree x

takeTree :: Int -> Tree a -> Tree a
takeTree 0 _            = Leaf 
takeTree n Leaf         = Leaf
takeTree n (Node l x r) = Node (takeTree (n-1) l) x (takeTree (n-1) r)
 
replicateTree :: Int -> a -> Tree a 
replicateTree n = takeTree n . repeatTree

-- replicateTree 2 3
-- Node (Node Leaf 3 Leaf) 3 (Node Leaf 3 Leaf)

-- 6. Newton’s method for computing the square root of a (non-negative)
-- floating-point number n can be expressed as follows:
--   + start with an initial approx to the result;
--   + given the current approx a, the next approx is defined by the 
--     function next   a = (a+n/a)/2;
--   + repeat the 2nd step until the two most recent approxs are within some 
--     desired distance of one another, return point - most recent value.

approx :: Double
approx = 1.0

delta :: Double
delta = 0.00001

sqroot :: Double -> Double
sqroot n = head . filter (\x -> abs (x*x - n) < delta) $ iterate next approx
           where next a = (a + n/a) / 2

sqroot' :: Double -> Double
sqroot' n = snd (head (filter (\t -> (snd t) - (fst t) < 0.00001) zipped)) 
            where
            a       = [a | a <- iterate (\a -> (a + n / a) / 2.0) 1.0]
            zipped  = zip a (tail a)















