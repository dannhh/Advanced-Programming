import System.IO
import Data.Char
-- ========================== HANGMAN ===============================

hangman :: IO ()
hangman = do putStrLn "Think a word: "
             word <- sgetLine
             putStrLn "Try to guess it: "
             play word
             
-- The action sgetLine reads a line of text from the keyboard, 
-- echoing each character as a dash to keep the word secret:
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else 
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)
                    
-- The action getCh reads a single character from the keyboard, 
-- without echoing it to the screen:
getCh :: IO Char 
getCh = do hSetEcho stdin False 
           x <- getChar
           hSetEcho stdin True
           return x

-- The function play is the main loop, which requests and processes
-- the guesses until the game ends.    
play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                  putStrLn "You got it!!"
               else 
                  do putStrLn (match word guess)
                     play word

-- The function match indicates which characters in one string occur
-- in a second string. For example, where match word guess :
-- > match "haskell" "pascal"
-- "-as--ll"
-- Note “haskell” matches l twice in “pascal”:                     
match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]
        
-- ============================= NIM ================================

-- Represent the player number (1 or 2) as an integer:
next :: Int -> Int
next 1 = 2
next 2 = 1

-- 
type Board = [Int]
initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

-- A move in the game is specified by a row number and the number of 
-- stars to be removed, and is valid if the row contains at least 
-- this many stars:
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

-- A valid move can then be applied to a board to give an new board 
-- by using a list comprehension to update the number of stars 
-- that remain in each row: 
move :: Board -> Int -> Int -> Board 
move board row num = [update r n | (r,n) <- zip [1..] board]
                     where update r n = if r == row then n-num else n
                     
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))
                    
putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e
                          
getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then        -- Data.Char
                        return (digitToInt x) -- Data.Char
                     else 
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

newline :: IO ()
newline = putChar '\n'

playNim :: Board -> Int -> IO ()
playNim board player = do newline 
                          putBoard board
                          if finished board then
                             do newline 
                                putStr "Player "
                                putStr (show (next player))
                                putStrLn " wins!!"
                          else 
                             do newline
                                putStr "Player "
                                putStr (show player)
                                row <- getDigit "Enter a row number: "
                                num <- getDigit "Stars to remove: "
                                if valid board row num then 
                                   playNim (move board row num) (next player)
                                else 
                                   do newline
                                      putStrLn "ERROR: Invalid move"
                                      playNim board player 

nim :: IO ()
nim = playNim initial 1                            
-- ============================ LIFE ================================

-- The game models a simple evolutionary system based on cells, played
-- on a two-dimensional board. Each square on theboard is either empty,
-- or contains a single living cell, each square on the board has eight 
-- immediate neighbours.

--    + a living cell survives if it has precisely two or three 
--      neighbouring squares that contain living cells,and
--    + an empty square gives birth to a living cell if it has 
--      precisely three neighbours that contain living cells, 
--      and remains empty otherwise.

-- SCREEN UTILITIES

-- Action that clears the screen
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

-- a function that displays a string at a given position by using 
-- control characters tomove the cursor to this position:
goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10
height :: Int
height = 10

type Board_LIFE = [Pos]
glider :: Board_LIFE 
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

-- display the living cells on the screen, and to decide if a given
-- position is alive or empty:
showcells :: Board_LIFE -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board_LIFE -> Pos -> Bool 
isAlive b p = elem p b

isEmpty :: Board_LIFE -> Pos -> Bool
isEmpty b p = not (isAlive b p)       

-- Function that returns the neighbours of a position:
neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x, y-1), (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

-- The auxiliary function wrap takes account of the wrapping around 
-- at the edges of the Board_LIFE
wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) + 1)

liveneighbs :: Board_LIFE -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- list of living positions that have precisely two or three living
-- neighbours, and hence survive to the next generation of the game:
survivors :: Board_LIFE -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

-- list of empty positions that have precisely 3 living neighbours, 
-- and hence give birth to a new cell, can be produced as follows:
{-
births :: Board_LIFE -> [Pos]
births b = [(x,y) | x <- [1..width], y <- [1..height],
                    isEmpty b (x,y), liveneighbs b (x,y) == 3]
-}
births :: Board_LIFE -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p, liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board_LIFE -> Board_LIFE
nextgen b = survivors b ++ births b
 
life :: Board_LIFE -> IO ()
life b = do cls 
            showcells b
            wait 500000
            life (nextgen b)  

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

play_life :: IO ()
play_life = life glider
-- ==================================================================
-- ======================== EXERCISES ===============================
-- 1. redefine putStr using a list comprehension and the library
-- func sequence_ :: [IO a] -> IO ().
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- 2. Define a version of putBoard :: Board -> IO () that displays
-- nim boards ofany size
putBoard' :: Board -> IO ()
putBoard' b = putIter 1 b
              where putIter _ []     = return ()
                    putIter n (x:xs) = do putRow  n     x
                                          putIter (n+1) xs 

-- 3. Redefine the generalised version of putBoard using a list
-- comprehension and sequence_
putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow n x | (n, x) <- zip [1..] xs]

-- **4. Define an action adder :: IO () that reads a given number of
-- integers from the keyboard, one per line, and displays their sum
adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getInt
           addSum <- readNum n
           putStr $ "The total is " ++ show addSum ++ "\n"
           
readNum :: Int -> IO Int 
readNum 0 = return 0
readNum n = do x <- getInt
               rest <- readNum (n-1)
               return (x+rest)

getInt :: IO Int
getInt = do x <- getLine
            return ((read x) :: Int)

-- 5. Redefine adder using the function sequence that performs a  
-- list of actions and returns a list of the resulting values.
adder' :: IO ()
adder' = do putStr "How many numbers? "
            n <- getInt 
            addList <- sequence [getInt | _ <- [0..n-1]]
            putStr $ "The total is " ++ show (sum addList) ++ "\n"


-- 6. Using getCh, define "readLine :: IO String" in the same way as 
-- getLine, except that it also permits the delete key to be used to
-- remove characters
getCh' :: IO Char
getCh' = do hSetEcho stdin False
            x <- getChar
            hSetEcho stdin True
            return x

readLine' :: IO String
readLine' = readLine'' ""

readLine'' :: [Char] -> IO [Char]
readLine'' xs = do x <- getCh
                   case x of '\n' -> return xs
                             '\DEL' -> if null xs
                                           then readLine'' ""
                                           else do putStr "\b \b"
                                                   readLine'' (init xs)
                             _ -> do putChar x
                                     readLine'' (xs ++ [x])
























