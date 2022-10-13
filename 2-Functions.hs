-- Q1
addDigit :: Int -> Int-> Int
addDigit num digit = num * 10 + digit

-- Q2
celciusToFahrenheit :: Double -> Double
celciusToFahrenheit celcius = celcius * 9 / 5 + 32

-- Q3
type Vertex = (Float, Float)
distance :: Vertex -> Vertex -> Float
distance (x1, y1) (x2, y2) = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- Q4
triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea vertex1 vertex2 vertex3 = sqrt(s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2
    a = distance vertex1 vertex2
    b = distance vertex2 vertex3
    c = distance vertex1 vertex3

-- Q5
isPrime :: Int -> Bool
isPrime num
  | num <= 1  = False
  | otherwise = null [factor | factor <- [2 .. floor(sqrt(fromIntegral num))], num `mod` factor == 0]

-- Q6
fact :: Int -> Int
fact num
  | num <= 1  = 1
  | otherwise = num * fact(num - 1)

-- Q7
perm :: Int -> Int -> Int
perm n 0 = 1
perm n r = (n - r + 1) * perm n (r - 1)

-- Q8
choose :: Int -> Int -> Int
choose n r
  | n == r    = 1
  | otherwise = n `div` (n - r) * choose (n - 1) r

-- Q9
remainder :: Int -> Int -> Int
remainder dividend divisor 
  | dividend < divisor = dividend
  | otherwise          = remainder (dividend - divisor) divisor

-- Q10
quotient :: Int -> Int -> Int
quotient dividend divisor
  | dividend < divisor = 0
  | otherwise          = 1 + quotient (dividend - divisor) divisor

-- Q11
binary :: Int -> Int
binary num
  | num < 2   = num
  | otherwise = num `mod` 2 + binary (num `div` 2) * 10

baseN :: Int -> Int -> Int
baseN num base
  | num < base = num
  | otherwise  = num `mod` base + baseN (num `div` base) base * 10

-- Q12
add :: Int -> Int -> Int
add 0 num2    = num2
add num1 num2 = succ (add (pred num1) (succ num2)) 

larger :: Int -> Int -> Int
larger 0 num2    = num2
larger num1 num2 = succ (larger (pred num1) (pred num2))

-- Q13
chop :: Int -> (Int, Int)
chop num
  | num < 10  = (0, num)
  | otherwise = (quotient + 1, remainder)
    where
      (quotient, remainder) = chop (num - 10)

-- Q14
concatenate :: Int -> Int -> Int
concatenate num1 num2 
  | num2 < 10 = num1 * 10 + num2 
  | otherwise = concatenate num1 quotient * 10 + remainder
    where
      (quotient, remainder) = chop num2

-- Complexity analysis
-- Number of multiplications: O(# digits of num2)
-- Number of subtractions:    ?

-- Q15
fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
-- Recursive definition (O(2 ^ n))
    -- | otherwise = fib (n - 1) + fib (n - 2)
-- Dynamic programming definition (O(n))
fib n = fib' (n - 1) 1 0
  where
    fib' :: Int -> Int -> Int -> Int
    fib' 0 curr prev = curr
    fib' numCalls curr prev = fib' (numCalls - 1) (curr + prev) curr

-- Q16
goldenRatio :: Float -> Float 
goldenRatio threshold = goldenRatio' threshold 2 1 1
  where
    goldenRatio' :: Float -> Int -> Int -> Float -> Float
    goldenRatio' threshold currFib prevFib prevRatio
      | abs (currRatio - prevRatio) < threshold = currRatio
      | otherwise                               = goldenRatio' threshold (currFib + prevFib) currFib currRatio
      where   
        currRatio = fromIntegral currFib / fromIntegral prevFib
