-- 5.1 カリー化関数
multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z
multTwoWithNine = multThree 9

compareWithHundred :: Int -> Ordering
compareWithHundred = compare 100
-- この関数は下記関数と等価
-- 下記はOrderingを直接返し、数を引数に取りOrderingを返す部分適用された関数(compare)を返す
-- compareWithHundred x = compare 100 x
                  
-- 5.1.1 セクション (という名のセクション)
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- 5.2 高階実演
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- 5.2.1 zipWithを実装する
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6]] [[3,4,5], [7,8,9]]
-- [[3,8,15],[21,40,54]]
-- 部分適用されたzipWith'がzipWith' に渡されている点に注意

-- 5.2.2 flipを実装する
flip' :: (a -> b -> c) -> (b -> a -> c)
--flip' f = g
--          where g x y = f y x
flip' f x y = f y x

-- 5.3.1 map関数
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- 5.3.2 filter関数
-- true/falseを返却する関数を述語という
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- ★Ord aがないとソートできないよエラーが出る★
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
          let smaller = filter (<= x) xs
              bigger = filter (> x) xs
          in quicksort smaller ++ [x] ++ quicksort bigger
           
-- 5.3.3. mapとfilterのさらなる例
largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

-- コラッツ列
--   任意の自然数から開始
--   数が1ならば終了
--   数が偶数なら2で割る
--   数が奇数なら3倍して1を足す
--   新しい値でこのアルゴリズムを繰り返す
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
-- where 版
-- numLongChains = length (filter isLong (map chain [1..100]))
--                where isLong xs = length xs > 15
-- let版
numLongChains =
                let isLong xs = length xs > 15
                                in length (filter isLong (map chain [1..100]))

-- 1から100までの数のうち、長さ15以上のコラッツ列の開始数になるものはいくつあるか?
howManyCratz :: Int
howManyCratz = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- 5.4 ラムダ式
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15)
                        (map chain [1..100]))

-- ラムダのパターンマッチ例
-- map (\(a,b) -> a + b) [(1,2), (2,3), (3,4)]

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- 5.5.1 foldlで左畳み込み
sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs
-- カリー化による簡易バージョン
sum' = foldl (+) 0

-- 5.5.2 foldrで右畳み込み
map'' :: (a -> b) -> [a] -> [b]
--map'' f xs = foldr (\x acc-> f x : acc) [] xs
-- カリー化による簡易バージョン
map'' f = foldr (\x acc-> f x : acc) []


elem' :: (Eq a) => a -> [a] -> Bool
--elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys
elem' y = foldr (\x acc -> if x == y then True else acc) False

-- 5.5.3: foldl1とfoldr1関数
maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

-- 5.5.4 幾つかの畳込み例
reverse' :: [a] -> [a]
--reverse' = foldl (\acc x -> x : acc) []
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- 5.5.6 無限リストを畳み込む
and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

-- 5.5.7 スキャン
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- 5.7.1 多引数関数の関数合成
gousei :: Double
gousei =  sum . replicate 5 $ max 6.7 8.9

gousei2 :: [Int]          
--gousei2 = replicate 2 (product (map (*3) $ zipWith max [1,2] [4,5]))
gousei2 = replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

-- 5.7.2 ポイントフリースタイル
sum'' :: (Num a) => [a] -> a
--sum'' xs = foldl (+) 0 xs
sum'' = foldl (+) 0

-- fn x = ceiling (negate (tan (cos (max 50 x))))
-- fn x = ceiling . negate . tan . cos $ max 50 x
fn = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
--oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]
