-- Get first element:
> head [1,2,3,4,5]
1
-- Remove first element:
> tail [1,2,3,4,5]
[2,3,4,5]

-- Select the first n elements of a list:
> take 3 [1,2,3,4,5]
[1,2,3]

-- Select the nth element of a list:
> [1,2,3,4,5] !! 2
3

-- Remove first n elements of a list:
> drop 3 [1,2,3,4,5]
[4,5]

-- Calculate the length of a list:
> length [1,2,3,4,5]
5

-- Calculate the sum of a list of numbers:
> sum [1,2,3,4,5]
15

-- Append two lists:
> [1,2,3] ++ [4,5]
[1,2,3,4,5]

-- Reverse a list:
> reverse [1,2,3,4,5]
[5,4,3,2,1]

-- Split a list:
> splitAt 3 [1,2,3,4,5]
([1,2,3], [4,5])

-- quot is integer division truncated toward zero
-- rem is integer modulus, satifying: (x `quot` y)*y + (x `rem` y) == x
> 5 `rem` (-3)
2
-- div is integer division truncated toward negative infinity
-- mod is integer modulus, satifying: (x `div` y)*y + (x `mod` y) == x
MOD and REM: 
> 5 `mod` (-3)
-1


