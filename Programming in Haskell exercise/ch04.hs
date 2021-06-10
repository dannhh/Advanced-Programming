-- ============================================================
-- splits an  even-lengthed list into two halves
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2
           
-- ============================================================
-- returns the third element in a list using head and tail
third1 :: [a] -> a
third1 xs = head (tail (tail xs))

-- returns the third element in a list using list indexing !!
third2 :: [a] -> a
third2 xs = xs !! 3

-- returns the third element in a list using pattern matching
third3 :: [a] -> a
third3 (_:_:third:_) = third

-- ============================================================
-- 'safetail' behaves in the same way as tail except thatit 
-- maps the empty list to itself rather than producing an error

-- using a conditional expression
safetail1 :: [a] -> [a]
safetail1 xs = if null xs
               then []
               else tail xs
               
-- using guarded equations
safetail2 :: [a] -> [a]
safetail2 xs | null xs = []
             | otherwise = tail xs
             
-- using pattern matching
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs -- safetail3 (x:xs) = xs

-- ============================================================
-- how the disjunction operator || can be defined in four 
-- different ways using pattern matching.

(||) :: Bool -> Bool -> Bool
True || False = True
True || True = True
False || True = True
False || False = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
False |||| a = a
_ |||| _ = True

(|||||) :: Bool -> Bool -> Bool
b ||||| c  | b == c = b
           | otherwise = True

-- ============================================================
-- how the meaning of the followingpattern  matching  
-- definition for  logical  conjunction &&  can  be 
-- formalised  using  conditionalexpressions
(&&) :: Bool -> Bool -> Bool
x && y = if x == True
           then if y == True
             then True
           else False
         else False

(&&&) :: Bool -> Bool -> Bool
x &&& y = if x == True
            then y
          else False

-- ============================================================
-- Multiple of 3 numbers type Int using lambda expression
mult :: Int -> Int -> Int -> Int 
mult = \x -> \y -> \z -> x * y * z

-- ============================================================
-- The Luhn algorithm: consider each digit as a separate number;
-- moving left, double every other number from the second last;
-- subtract 9 from each number that is now greater than 9;
-- add all the resulting numbers together;
--     if the total is divisible by 10, the card number is valid.

-- Define a function luhnDouble :: Int -> Int that doubles a digit
-- and subtracts 9 if the result is greater than 9. 
luhnDouble :: Int -> Int
luhnDouble a | a * 2 > 9 = a * 2 - 9
             | otherwise = a * 2

-- define a function luhn :: Int ->Int -> Int -> Int -> Bool 
-- that decides if a four-digit bank card number is valid
luhn :: Int ->Int -> Int -> Int -> Bool
luhn x y z t = total `mod` 10 == 0
               where total = (luhnDouble x) + y + (luhnDouble z) + t







