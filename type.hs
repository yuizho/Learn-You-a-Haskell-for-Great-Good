-- 2.1
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

functorial :: Integer -> Integer
functorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r                 
