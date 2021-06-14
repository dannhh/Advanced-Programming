-- ================== HIGHER-ORDER FUNCTIONS ========================
import Prelude hiding (map, filter, foldr, sum, length, reverse, (.),
 id, all, any, takeWhile, dropWhile, curry, uncurry)
-- ====================== BASIC CONCEPTS ============================
-- normal 
add :: Int -> Int -> Int 
add x y = x + y
-- means
add' :: Int -> (Int -> Int) 
add' = \x -> (\y -> x + y)

-- twice
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- ===================== PROCESSING LISTS ===========================
-- map applies a function to all elements of a list
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

-- map' applies a function to all elements of a list using recursion
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = (f x) : (map' f xs)

-- filter, which selects all elements of a list that satisfy a 
-- predicate, defined using a list comprehension
filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x <- xs, f x == True]

-- filter, which selects all elements of a list that satisfy a 
-- predicate, defined using recursion
filter' :: (a -> Bool) -> [a] -> [a]
filter' f []                   = []
filter' f (x:xs) | f x == True = x : filter' f xs
                 | otherwise   = filter' f xs 

-- sum of the squares of the even integers from a list
sumsqeven :: [Int] -> Int
sumsqeven xs = sum [x ^ 2 | x <- xs, even x]

sumsqeven' :: [Int] -> Int
sumsqeven' xs = sum (map (^2) (filter even xs))

-- ===================== THE FOLDR FUNCTION =========================
-- the function maps the empty list to a value v, and any non-empty 
-- list to an operator # applied to the head of the list and the 
-- result of recursively processing the tail

-- normal f
-- f []     = v
-- f (x:xs) = x # f xs 

-- normal sum
sum []     = 0
sum (x:xs) = x + sum xs

-- sum fold right
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

sum'' xs = foldr (+) 0 xs

-- foldr function defined using recursion:
-- sum [1,2,3] = foldr (+) 0 [1,2,3] = 1+(2+(3+0)) = 6
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []     = v
foldr f v (x:xs) = f x (foldr f v xs)

length :: [a] -> Int
length = foldr (\_ n -> n + 1) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse :: [a] -> [a]
reverse = foldr snoc []

reverse' [a] = [a] 
reverse' (x:xs) = snoc x (reverse' xs)

-- ================ THE COMPOSITION OPERATOR ========================

-- The higher-order library operator . returns the composition of 
-- two functions as a single function
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

-- normal
odd :: Int -> Bool 
odd n = not (even n)
-- twice f x = f (f x)
-- sumsqreven = sum (map (^2) (filter even ns))
-- rewritten by composition operator
odd' :: Int -> Bool
odd' = not . even
-- twice = f . f
-- sumsqreven = sum . map (^2) . filter even

id :: a -> a
id = \x -> x

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

-- ========================= EXERCISES ==============================

-- show how the list comprehension [f x | x <- xs, p x] can be 
-- re-expressed using the higher-order functions map and filter
func f p xs = map f (filter p xs)

-- decide if all elements of a list satisfy a predicate
-- all (<10) [1,3,5,7,9]
all :: (a -> Bool) ->[a] -> Bool
all p = and . map p

-- decide if any element of a list satisfies a predicate:
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- select elements from a list while they satisfy a predicate:
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p []                   = []
takeWhile p (x:xs) | p x == True = x : takeWhile p xs
                   | otherwise   = []
                   
-- Remove elements from a list while they satisfy a predicate:
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p []                    = []
dropWhile p (x:xs) | p x == False = x : xs
                   | otherwise    = dropWhile p xs

-- *Redefine the functions map f and filter p using foldr
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x xs -> f x : xs) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x xs -> if (f x) then x : xs else xs) []

-- Using foldl, define a function dec2int :: [Int] -> Int that
-- converts a decimal number into an integer 
-- > dec2int [2,3,4,5]
-- 2345
-- [2,3,4,5]
-- 2*1000 + 3*100 + 3*10 + 5
-- ((2*10+3)*10+4)*10+5
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0 

-- define the higher-order library function curry that converts a 
-- function on pairs into a curried function, and, conversely, 
-- the function uncurry that converts a curried function with two 
-- arguments into a function on pairs.

-- curry converts an uncurried function to a curried function
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y

-- The function unfold p h t produces the empty list if the predicate
-- p is true, and otherwise produces a non-empty list by applying the
-- function h to give the head, and the function t to generate another
-- argument that is recursively processed in the same way to produce
-- the tail of the list. 

unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
        
-- For example, the function int2bin can be rewritten more compactly
-- using unfold as follows: 
--            int2bin = unfold (== 0) (‘mod‘ 2) (‘div‘ 2)

-- Redefine the functions chop8 using unfold.
chop8' :: [Int] -> [[Int]]
chop8' = unfold (== []) (take 8) (drop 8)

-- Redefine the functions map f using unfold.
map''' :: (a -> b) -> [a] -> [b]
map''' f = unfold (null) (f.head) (tail)

-- Redefine the functions iterate f using unfold.
iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\x -> False) (id) (f)

------------------------- bonus -------------------------------------
{-type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x acc -> x + 2 * acc) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

addParity :: [Bit] -> [Bit]
addParity bits =  parityBit : bits
                  where parityBit = if oddOnes bits then 1 else 0

oddOnes :: [Bit] -> Bool
oddOnes bits = length ones `mod` 2 /= 0
               where ones = filter (==1) bits

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity (parityBit:xs) = case (oddOnes xs, parityBit) of
                               (True, 1)  -> xs 
                               (False, 0) -> xs
                               (_, _)     -> error "Your data was corrupted"  

decode :: [Bit] -> String
decode bits = map (chr . bin2int) (map checkParity (chop9 bits))

transmit :: String -> String
transmit = decode . corruptChannel . encode

channel :: [Bit] -> [Bit]
channel = id

corruptChannel :: [Bit] -> [Bit]
corruptChannel = tail
-}
----------------------- end bonus -----------------------------------

-- Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] 
-- For example: > altMap (+10) (+100) [0,1,2,3,4]
--              [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = []
altMap f g (x:xs) = f x : altMap g f xs

-- Using altMap, define a function luhn :: [Int] -> Bool that 
-- implements the Luhn algorithm for bank card numbers of any length.
-- Test your new function using your own bank card
luhnDouble :: Int -> Int
luhnDouble a | a * 2 > 9 = a * 2 - 9
             | otherwise = a * 2

luhn :: [Int] -> Bool
luhn xs = if total `mod` 10 == 0 then True else False
          where total = altMap id luhnDouble (reverse xs)
