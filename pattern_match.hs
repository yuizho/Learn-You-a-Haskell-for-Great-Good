lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "SOrry, you're out of luck, pal!"

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"

--3.1.1               
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)               

third :: (a, b, c) -> c
third (_, _, z) = z

--3.1.2
head' :: [a] -> a
head' [] = error "Can7t call head on an empty list, dummy!"
head' (x:_) = x

--3.1.3
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "the first letter of " ++ all ++ " is " ++ [x]

--3.2
-- Guard
bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "you're a while, congratulations!"

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "you're a while, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

-- 3.3 where?!
-- weight / height ^2の記述をwhereで変数に束縛する
bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise   = "you're a while, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- 3.3.2 パターンマッチとwhere
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
                              where (f:_) = firstname
                                    (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
              where bmi weight height = weight / height ^ 2

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h^2]

-- 3.4 letItBe
cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
        in sideArea + 2 * topArea

-- 3.5 case 式
head'' :: [a] -> a

head'' [] = error "No head for empty lists!"

head'' (x:_) = x

-- 3.5 case 式'

describeList :: [a] -> String
describeList ls = "The list is "
                  ++ case ls of [] -> "empty."
                                [x] -> "a singleton list."
                                xs -> "a long list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
                  where what [] = "empty."
                        what [x] = "a singleton list."
                        what xs = "a longer list."


