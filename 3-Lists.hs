import Data.Char
import Data.List

-- 3.1 Basics
-- Q3
-- Pre: the element to be indexed is in the list
pos :: Eq a => a -> [a] -> Int
pos n (x:xs)
  | n == x    = 0
  | otherwise = 1 + pos n xs

-- Q4
twoSame :: [Int] -> Bool
twoSame [] = False
twoSame (x:xs)
  | x `elem` xs = True
  | otherwise   = twoSame xs

-- Q5
rev1 :: [a] -> [a]
rev1 [] = []
rev1 (x:xs) = rev1 xs ++ [x]

rev2 :: [a] -> [a]
rev2 xs = rev2' xs []
  where
    rev2' :: [a] -> [a] -> [a]
    rev2' [] ys = ys
    rev2' (x:xs) ys = rev2' xs (x:ys)

-- Q6
substring1 :: String -> String -> Bool
substring1 "" s2 = True
substring1 s1 "" = False
substring1 s1 s2@(c2:c2s) = prefix1 s1 s2 || substring1 s1 c2s
  where
    prefix1 :: String -> String -> Bool
    prefix1 "" s2 = True
    prefix1 s1 "" = False
    prefix1 (c1:c1s) (c2:c2s) = c1 == c2 && prefix1 c1s c2s

    prefix2 :: String -> String -> Bool
    prefix2 "" s2 = True
    prefix2 s1 "" = False
    prefix2 s1 s2 = s1 == take (length s1) s2

substring2 :: String -> String -> Bool
substring2 s1 "" = False
substring2 s1 s2@(_:c2s)
  | length s1 > length s2     = False
  | s1 == take (length s1) s2 = True
  | otherwise                 = substring2 s1 c2s

-- Q7
-- Pre: all three strings are of the same length
transpose :: String -> String -> String -> String
transpose s a1 a2 = transpose' s idxs
  where
    idxs = map (`pos` a1) a2

    transpose' :: String -> [Int] -> String
    transpose' s [] = []
    transpose' s (i:is) = c : transpose' s is
      where
        c = s !! i

-- Q8
removeWhitespace :: String -> String
removeWhitespace "" = ""
removeWhitespace s@(c:cs)
  | isSpace c = removeWhitespace cs
  | otherwise = s

-- Q9
nextWord :: String ->  (String, String)
nextWord "" = ("", "")
nextWord (c:cs)
  | isSpace c = ("", removeWhitespace cs)
  | otherwise = (c:word, rest)
    where
      (word, rest) = nextWord cs

-- Q10
splitUp :: String -> [String]
splitUp "" = []
splitUp s = word:splitUp rest
  where
    (word, rest) = nextWord (removeWhitespace s)

-- Q11
-- Pre: n >= 1
primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2
  where
    primeFactors' :: Int -> Int -> [Int]
    primeFactors' 1 p = []
    primeFactors' n p
      | n `mod` p == 0 = p : primeFactors' (n `div` p) p
      | p == 2         = primeFactors' n 3
      | otherwise      = primeFactors' n (p + 2)

-- Q12
hcf :: Int -> Int -> Int
hcf a b = a `div` product (ps \\ ps')
  where
    ps = primeFactors a
    ps' = primeFactors b

-- Using Euclid's algorithm
hcf2 :: Int -> Int -> Int
hcf2 a 0 = a
hcf2 a b = hcf b (a `mod` b)

-- Q13
lcm :: Int -> Int -> Int
lcm a b = a * product (ps' \\ ps)
  where
    ps = primeFactors a
    ps' = primeFactors b

-- 3.2 List comprehensions
-- Q3
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:rest) = quicksort smaller ++ [pivot] ++ quicksort larger
  where
    smaller = [element | element <- rest, element <= pivot]
    larger = [element | element <- rest, element > pivot]
