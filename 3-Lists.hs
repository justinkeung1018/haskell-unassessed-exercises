-- 3.1 Basics
-- Q6
isSubstring :: String -> String -> Bool
isSubstring string1 "" = False
isSubstring string1 string2@(char2:char2s)
  | isPrefix string1 string2 = True
  | otherwise                = isSubstring string1 char2s
  where
    isPrefix :: String -> String -> Bool
    isPrefix "" string2 = True
    isPrefix (char1:char1s) (char2:char2s) = char1 == char2 && isPrefix char1s char2s

isSubstring2 :: String -> String -> Bool
isSubstring2 string1 string2@(char2:char2s)
  | length string1 > length string2          = False
  | string1 == take (length string1) string2 = True
  | otherwise                                = isSubstring2 string1 char2s

-- 3.2 List comprehensions
-- Q3
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (pivot:rest) = quicksort smaller ++ [pivot] ++ quicksort larger
  where
    smaller = [element | element <- rest, element <= pivot]
    larger = [element | element <- rest, element > pivot]
