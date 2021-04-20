import Data.Char
import Data.List

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

-- Binary string transmitter
type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
    where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 ::[Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id


-- Voting algorithms
-- First past the post
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter(== x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Exercises
dec2int :: [Int] -> Int
dec2int = foldl(\x y -> 10*x + y) 0

altMap :: (a -> b) -> (a->b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs
