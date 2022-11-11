import Data.Char
import Data.List

-- Q1
-- (a)
depunctuate :: String -> String
depunctuate = filter (`elem` ".,:")

-- (b)
makeString :: [Int] -> String
makeString = map chr

-- (c)
enpower :: [Int] -> Int
enpower = foldr (^) 1

-- (d)
revAll :: [[a]] -> [a]
revAll = concatMap reverse

-- (e)
rev :: [a] -> [a]
rev = foldl (flip (:)) []

-- (f)
dezip :: [(a, b)] -> ([a], [b])
dezip = foldr prepend ([], [])
  where
    prepend (x, y) (xs, ys) = (x : xs, y : ys)

-- Q2
allSame :: [Int] -> Bool
allSame ls = and (zipWith (==) ls (tail ls))

-- Q3
-- (a)
factorials :: [Int]
factorials = scanl (*) 1 [2..]

-- (b)
e :: Float
e =  sum (take 5 (map (1 /) (scanl (*) 1 [1..])))

-- Q4
squash1 :: (a -> a -> b) -> [a] -> [b]
squash1 f [] = []
squash1 f [x] = []
squash1 f (x1 : x2 : xs) = f x1 x2 : squash1 f (x2 : xs)

squash2 :: (a -> a -> b) -> [a] -> [b]
squash2 f xs = zipWith f xs (tail xs)

-- Q5
converge :: (a -> a -> Bool) -> [a] -> a
converge f [x] = x
converge f (x1 : x2 : xs)
  | f x1 x2   = x1
  | otherwise = converge f (x2 : xs)