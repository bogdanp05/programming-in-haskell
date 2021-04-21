import Data.Char
--import Data.List
import System.IO

-- Hangman, chapter 10.6
hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else 
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                    putStrLn "You got it!!"
               else
                    do putStrLn (match word guess)
                       play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]
     

-- Exercise 4
readInt :: IO Int
readInt = do
            x <- getLine
            return (read x :: Int)

readInts :: Int -> IO [Int]
readInts 0 = return []
readInts n = do 
                x <- readInt
                xs <- readInts (n-1)
                return (x:xs) 

adder :: IO ()
adder = do 
    putStr "How many numbers? "
    n <- readInt
    xs <- readInts n
    putStrLn (show (sum xs))
    return ()
  
