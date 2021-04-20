double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
-- Average of a list of integers
average ns = sum ns `div` length ns

n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

second :: [a] -> a
second xs = head (tail xs)


halve :: [a] -> ([a], [a])
halve xs = ((take n xs), (drop n xs))
    where n = length xs `div` 2


luhnDouble :: Int -> Int
luhnDouble x | x*2 <= 9 = x*2
             | otherwise = x*2 - 9


luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z t = if s `mod` 10 == 0 then True else False
    where s =  luhnDouble x + y  + luhnDouble z  + t  
