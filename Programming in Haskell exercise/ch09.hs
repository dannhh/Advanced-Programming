-- ================== ARITHMETIC OPERATORS ==========================

-- declaring a type for the four arithmetic operators
data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- decides if the application of an operator to two positive 
-- naturals gives another positive natural
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

-- function apply that actually performs such a validapplication:
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- ==================== NUMERIC EXPRESSIONS =========================

data Expr = Val Int | App Op Expr Expr

instance Show Expr where 
    show (Val n)     = show n
    show (App o l r) = brak l ++ show o ++ brak r
                       where brak (Val n) = show n
                             brak e       = "(" ++ show e ++ ")"
-- we define a function that returns the list of values in an expr
values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

-- function eval that returns the overall value of an expression, 
-- provided that this value is a positive natural number:
eval :: Expr -> [Int]
eval (Val n)      = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]       
                                          
-- ================== COMBINATORIAL FUNCTIONS =======================
-- > subs [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
             
-- > interleave 1 [2,3,4]
-- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
 
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

-- > perms [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- > choices [1,2,3]
-- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],
-- [2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- ================== FORMALISING THE PROBLEM =======================
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- e :: Expr represents the expression (1 + 50)  (25 – 10), then we have:
-- > solution e [1,3,7,10,25,50] 765
-- True

-- ==================== BRUTE FORCE SOLUTION ========================
split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls,rs) <- split xs]

-- > split [1,2,3,4]
-- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns, 
                 l       <- exprs ls, 
                 r       <- exprs rs,  
                 e       <- combine l r]                          

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div] 

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- ======================= PERFORMANCE TESTING ======================
main :: IO()
main = print (solutions [1,3,7,10,25,50] 765)

-- ghc -O2 ch09.hs
-- ./ch09
-- =============== COMBINING GENERATION AND EVALUATION ==============

type Result = (Expr,Int)

-- For the empty list there are no possible results, while for a single
-- number there is a single result formed from that number, provided 
-- that the number itself is a positive natural number. Otherwise, for 
-- two or more numbers we first produce all splittings of the list, then 
-- recursively calculate all possible results for each of these lists, 
-- and, finally, combine each pair of results using each of the four 
-- numeric operators that are valid:
results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                     lx      <- results ls,
                     ry      <- results rs,
                     res     <- combine' lx ry]
                    
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- ================== EXPLOITING ALGEBRAIC PROPERTIES ===============

-- The function solutions’ generates all possible expressions over the
-- given numbers whose evaluation issuccessful, but in practice many of
-- these expressions will be essentially the same, due to the fact that
-- thenumeric operators have algebraic properties
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

-- ==================================================================
-- ============================= EXERCISES ==========================

-- 1.
choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys]

-- 2.
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice (x:xs) (y:ys)  = elem x ys && isChoice xs (removeFirst x ys)

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys)  | x == y = ys
                      | otherwise = y : removeFirst x ys

-- 3.
-- solutions won't terminate

-- 4.
possibleExpr :: [Int] -> [Expr]
possibleExpr = concat . map exprs . choices

numberOfPossibleExpressions :: [Int] -> Int
numberOfPossibleExpressions = length . possibleExpr
--numberOfPossibleExpressions [1,3,7,10,25,50]
--33665406

--valid :: Op -> Int -> Int -> Bool
--valid Add _ _ = True
--valid Sub x y = x > y
--valid Mul _ _ = True
--valid Div x y = x `mod` y == 0

successfullExpr :: [Int] -> [[Int]]
successfullExpr = filter (not . null) . map eval . possibleExpr

numberOfSuccessfullExpressions :: [Int] -> Int
numberOfSuccessfullExpressions = length . successfullExpr 
--numberOfSuccessfullExpressions [1,3,7,10,25,50]
--4672540

-- 5.
--valid :: Op -> Int -> Int -> Bool
--valid Add _ _ = True
--valid Sub x y = True 
--valid Mul _ _ = True
--valid Div x y = y /= 0 && x `mod` y == 0

--numberOfSuccessfullExpressions [1,3,7,10,25,50]
--10839369

-- 6.
-- a.
--data Op = Add | Sub | Mul | Div | Pow
--instance Show Op where
--  show Add = "+"
--  show Sub = "-"
--  show Mul = "*"
--  show Div = "/"
--  show Pow = "^"

--valid :: Op -> Int -> Int -> Bool
--valid Add x y = x <= y
--valid Sub x y = x > y
--valid Mul x y = x /= 1 && y /= 1 && x <= y
--valid Div x y = y /=0 && y /= 1 && x `mod` y == 0
--valid Pow x y = x <= y && y >= 0

--ops = [Add, Sub, Mul, Div, Pow]

-- b.
-- c.




































