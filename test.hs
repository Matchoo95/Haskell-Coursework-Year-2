import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks :: [StudentMark] -> [Int]
marks stMarks = [ mk | (st,mk) <- stMarks ]

pass :: [StudentMark] -> [String]
pass stMarks = [ st | (st,mk) <- stMarks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
    | x <= y            = (x,y)
    | otherwise         = (y,x)

-- Tuples
-- 1)
sumDifference :: Int -> Int -> (Int,Int)
sumDifference a b = ((a+b),(a-b))
-- different brackets
sumDifference2 :: Int -> Int -> (Int, Int)
sumDifference2 x y = (x+y, x-y)

-- 2.
grade :: StudentMark -> Char
grade (s,m)
    | m >= 70 = 'A'
    | m >= 60 = 'B'
    | m >= 50 = 'C'
    | m >= 40 = 'D'
    | m < 40 = 'F'
    | otherwise = error "Error"

-- 3.
capMark :: StudentMark -> StudentMark
capMark (s,m)
    | m >= 40 = (s,40)
    | otherwise = (s,m)

-- Lists and Strings
-- 4.
firstNumbers :: Int -> [Int]
firstNumbers x = [1..x]

-- 5.
firstSquares :: Int -> [Int]
firstSquares x = [ n^2 | n <- firstNumbers x]

-- 6.
capitalise :: String -> String
capitalise x = [toUpper n | n <- x]

-- 7.
onlyDigits :: String -> String
onlyDigits x = [ n | n <- x, n `elem` "123456789"]

-- 8.
capMarks :: [StudentMark] -> [StudentMark]
capMarks mk = [capMark x | x <- mk]

-- 9.
gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents mkList = [(st, grade (st, mk)) | (st, mk) <- mkList]

-- 10.
duplicate :: String -> Int -> String
duplicate x y
    | y == 0 = []
    | otherwise = x ++ duplicate x (y - 1)

-- 11.
divisors :: Int -> [Int]
divisors x = [ n | n <- [1..x], mod x n == 0]

-- 12.
isPrime :: Int -> Bool
isPrime x = divisors x == [1, x]

-- 13.
split :: [(a,b)] -> ([a],[b])
split [] = ([], [])
split ((x,y):xs) = (x:(fst result), y:(snd result))
    where result = split xs

split2 :: [(a,b)] -> ([a],[b])
split2 list = ([x | (x,y) <- list], [y | (x,y) <- list])

addFilms fanName filmName database = map(\(Film title director year fans) -> if(title == filmName) then (Film title director year fans++(fanName))) else (Film title director year fans))
