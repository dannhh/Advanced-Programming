-- ================= DECLARING TYPES AND CLASSES ====================
import Prelude hiding (Maybe, Nothing, Just)
-- ===================== TYPE DECLARATIONS ==========================

-- type mechanism of Haskell
-- name begin with capital letter 
type String = [Char]
type Pos = (Int, Int)
type Trans = Pos -> Pos

-- declaration cannot be recursive
-- type Tree = (Int, [Tree])

-- type declarations can also be parameterised by other types
type Pair a = (a, a)
type Assoc k v = [(k, v)]

-- a function that returns the first value that is associated with a 
-- given key in a table
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- ===================== DATA DECLARATIONS ==========================

-- specifying values using the data mechanism of Haskell.
data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East  (x,y) = (x+1,y)
move West  (x,y) = (x-1,y)

-- The constructors in a data declaration can also have arguments
data Shape = Circle Float | Rect Float Float

-- data declarations themselves can also be parameterised.
data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

-- =================== NEWTYPE DECLARATIONS =========================

-- If a new type has a single constructor with a single argument, 
-- then it can also be declared using the newtype mechanism.
newtype Nat' = N Int

-- means:
-- type Nat = Int
-- data Nat = N Int

-- ===================== RECURSIVE TYPES ============================

-- using the data and newtype mechanisms can also be recursive

data Nat = Zero | Succ Nat
-- Zero
-- Succ Zero
-- Succ (Succ Zero) ...

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

-- using recursion the function add can be redefined without the need 
-- for such conversions
add' :: Nat -> Nat -> Nat
add' Zero n     = n
add' (Succ m) n = Succ (add' m n)

data List a = Nil | Cons a (List a)
len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

-- store data in a two-way branching structure, or binary tree
--          5
--     3         7
--  1     4   6     9
data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int 
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- function that decides if a given value occurs in a tree:
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

-- a function that flattens a tree to a list
flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l y r) = flatten l ++ [y] ++ flatten r

-- BST: function that decides if a given value occurs in a tree:
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                 = x == y
occurs' x (Node l y r) | x == y    = True
                       | x < y     = occurs' x l
                       | otherwise = occurs' x r

-- =================== the rest don't study =========================


-- ======================= EXERCISES ================================

-- 1. In a similar manner to the function add, define a recursive 
-- mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
mult :: Nat -> Nat -> Nat 
mult Zero _        = Zero
mult _    Zero     = Zero 
mult m    (Succ n) = add m (mult m n)
-- 4 * 3 = 4 + (4 + (4 + 0))

-- 2. Redefine occurs (Tree) using ordered type LT, GT, EQ
occurs''' :: Ord a => a -> Tree a -> Bool
occurs''' x (Leaf y)     = x == y
occurs''' x (Node l y r) = case compare x y of
                           LT -> occurs''' x l
                           EQ -> True
                           GT -> occurs''' x r

-- 3. Define a function balanced :: Tree a -> Bool 
-- that decides if a binary tree is balanced or not
data BTree a = BLeaf a | BNode (BTree a) (BTree a)
height :: BTree a -> Int
height (BLeaf x)     = 1
height (BNode l r) = 1 + max (height l) (height r)

balanced :: BTree a -> Bool 
balanced (BLeaf y)   = True
balanced (BNode l r) = dif <= 1 && balanced l && balanced r
                       where dif = abs (height l - height r)
                         
btree :: BTree Int 
btree = BNode (BNode (BNode (BLeaf 1) (BLeaf 3)) (BLeaf 4)) (BLeaf 6)        


-- 4. Define a function balance :: [a] -> Tree a that converts 
-- a non-empty list into a balanced tree

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> BTree a
balance [x] = BLeaf x
balance xs  = BNode (balance l) (balance r)
              where (l, r) = halve xs

-- 5. data Expr = Val Int | Add Expr Expr
-- define a higher-order function 
--          folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
-- such that folde f g replaces each Val constructor in an expression 
-- by the function f, and each Add constructor by the function g.
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n)     =  f n
folde f g (Add e1 e2) =  g (folde f g e1) (folde f g e2)

-- 6. 
-- Using folde define a funciton
-- eval :: Expr -> Int
-- that evaluates an expression to an integer value, and a function 
-- size :: Expr -> Int
-- that calculates the number of values in an expression.

evalE :: Expr -> Int
evalE (Val n)     =  n
evalE (Add e1 e2) =  folde toEnum (+) (Add e1 e2)

numVals :: Expr -> Int
numVals (Val n)     =  1
numVals (Add e1 e2) =  numVals e1 + numVals e2  

-- 7. Complete the following instance declarations:
-- instance Eq a => Eq (Maybe a) where
-- instance Eq a => Eq [a] where

{-
	instance Eq a => Eq (Maybe a) where
	Just x    == Just y     = x == y
	Nothing   == Nothing    = True
	_         == _          = False

	instance Eq a => Eq [a] where
	[] == [] = True
	(x:xs) == (y:ys) = x == y && xs == ys
	_ == _ = False
-}

-- 8. Extend the tautology checker to support the use of logical
-- disjunction  and equivalence () inpropositions.

data Prp = Constantification Bool
         | Assignment Char
         | Negation Prp
         | Equivalence Prp Prp
         | Disjunction Prp Prp
         | Conjunction Prp Prp
         | Implication Prp Prp

-- A list of pairs of keys to their corresponding values. 
type Matches key value = [(key,value)]
type Substitution = Matches Char Bool
-- find :: Eq k => k -> Assoc k v -> v
-- find k t = head [v | (k', v) <- t, k == k']

-- Evaluates a proposition in terms of Substitution for its variables.
evalPrp :: Substitution -> Prp -> Bool
evalPrp _ (Constantification b)  =  b
evalPrp s (Equivalence p1 p2)    =  not (evalPrp s p1 || evalPrp s p2)
-- Look up the list of Substitution, 
-- and substitute the Bool value for the Char argument.
evalPrp s (Assignment c)         =  find c s 
evalPrp s (Negation p)           =  not (evalPrp s p)
evalPrp s (Disjunction p1 p2)    =  evalPrp s p1 || evalPrp s p2
evalPrp s (Conjunction p1 p2)    =  evalPrp s p1 && evalPrp s p2
-- Knowing that: False < True
evalPrp s (Implication p1 p2)    =  evalPrp s p1 <= evalPrp s p2 

-- Returns all the variables in a proposition.
variables :: Prp -> [Char]
variables (Constantification _)  =  [] 
variables (Assignment c)        =  [c]
variables (Equivalence p1 p2)   =  variables p1 ++ variables p2
variables (Negation p)          =  variables p
variables (Disjunction p1 p2)   =  variables p1 ++ variables p2
variables (Conjunction p1 p2)   =  variables p1 ++ variables p2
variables (Implication p1 p2)   =  variables p1 ++ variables p2

-- Returns all the possible boolean values.
boolsAll :: Int -> [[Bool]]
boolsAll 0 = [[]]
boolsAll n = map (False :) bss ++ map (True :) bss
             where bss = boolsAll (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- Generates all the possible substitutions for a given proposition.
substitutions :: Prp -> [Substitution]
substitutions p = map (zip vs) (boolsAll (length vs))
                  where vs = rmdups $ variables p

-- Decides whether a proposition is a tautology.
isItATautology :: Prp -> Bool
isItATautology p = and [evalPrp s p | s <- substitutions p]

-- 9. Extend the abstract machine to support the use of multiplication

data Exp = Const  Int 
         | Addi   Exp Exp
         | Mult  Exp Exp
         deriving Show

data Oper = EVAL Exp
          | AD   Int
          | MU   Int

type Controls = [Oper]

-- Evaluates an expression in the context of a control stack.
evalExp :: Exp -> Controls -> Int
evalExp (Const n)     c = execOper c n
evalExp (Addi  e1 e2) c = evalExp e1 (EVAL e2 : AD 0 : c)
evalExp (Mult  e1 e2) c = evalExp e1 (EVAL e2 : MU 1 : c)

{-
evalExp evaluates an expression in the context of a control stack. 
That is, if the exp is an integer, it is already fully evaluated, and
we begin executing the control stack. If the expression is an add or 
a mult, we avaluate the first argument x, placing the operation EVAL
y on top of the control stack to indicate that the second argument, y, 
should be evaluated once evaluation of the first argument is completed.
-}

-- Executes the control stack in the context of an integer operand.
execOper :: Controls -> Int -> Int
execOper [] n                    =  n
execOper (EVAL e1 : AD 0 : c) n  =  evalExp e1 (AD n : c)
execOper (EVAL e1 : MU 1 : c) n  =  evalExp e1 (MU n : c)
execOper (AD n : c) m            =  execOper c (n+m)
execOper (MU n : c) m            =  execOper c (n*m)

-- Calculates the value of an expression using a control stack.
valExp :: Exp -> Int
valExp e = evalExp e []

-- Values
exp0 = Const 0
exp1 = Const 1
exp2 = Addi exp0 exp1
exp3 = Mult (Const 5) (Const 5)
exp4 = Addi exp0 exp2
exp5 = Addi (Mult exp0 exp1) (Mult exp1 exp2)






































