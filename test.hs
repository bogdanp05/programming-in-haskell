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

