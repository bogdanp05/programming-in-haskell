insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)
      | otherwise = 1
      

index :: [a] -> Int -> a
index (x:xs) 0 = x
index (x:xs) n = index xs (n-1)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

halve :: [a] ->  ([a], [a])
halve xs = ((take n xs), (drop n xs))
    where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort xs | length xs == 1 = xs
         | otherwise = merge (msort ys) (msort zs)
    where (ys, zs) = halve xs
