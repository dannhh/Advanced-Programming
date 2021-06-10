
import Prelude hiding (product)

-- product
product :: Num t => [t] -> t
product []     = 1
product (x:xs) = x * product xs


-- quick sort: ascending --
qsortA :: Ord t => [t] -> [t]
qsortA []     = []
qsortA (x:xs) = qsortA smaller ++ [x] ++ qsortA larger
                where  smaller = [a | a <- xs, a <= x]
                       larger  = [b | b <- xs, b >  x]
                       

-- quick sort: descending --
qsortD :: Ord t => [t] -> [t]
qsortD []     = []
qsortD (x:xs) = qsortD larger ++ [x] ++ qsortD smaller
               where   smaller = [a | a <- xs, a <= x]
                       larger  = [b | b <- xs, b >  x]
                       

-- Q: quick sort ascending, replace '<=' to '<'
-- A: Duplicate values will be lost

