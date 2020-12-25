module Part3 where

import Part1

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 number = isPrime 2 number
    where
        isPrime :: Integer -> Integer -> Bool
        isPrime _ 1 = False
        isPrime divisor number
            | divisor * divisor > number = True
            | number `mod` divisor == 0 = False
            | otherwise = isPrime (divisor + 1) number

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
getPrimeDivisors :: Integer -> Integer -> [Integer]
getPrimeDivisors _ 1 = []
getPrimeDivisors divisor number
    | divisor * divisor > number = [number]
    | number `mod` divisor == 0 = divisor : getPrimeDivisors divisor (number `div` divisor)
    | otherwise = getPrimeDivisors (divisor + 1) number

groupList [] = []
groupList (x:xs) = groupLoop [x] x xs
    where
    groupLoop acc c [] = [acc]
    groupLoop acc c (y:ys)
        | y == c    = groupLoop (y:acc) c ys
        | otherwise = acc : groupLoop [y] y ys

prob19 num = map (\d -> (head d, length d)) (groupList (getPrimeDivisors 2 num))

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 a = a == sum (removeItem a (1 : getPrimeDivisors 2 a))

removeItem :: Integer -> [Integer] -> [Integer]
removeItem _ [] = []
removeItem x (y : ys)
  | x == y = removeItem x ys
  | otherwise = y : removeItem x ys

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
getAllDivisors :: Integral a => a -> [a]
getAllDivisors n = [a | a <- [1 .. (n -1)], n `rem` a == 0]
prob21 :: Integer -> [Integer]
prob21 number = (getAllDivisors number) ++ [number]

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = product ((map iCount) (words str))
    where
        iCount :: String -> Integer
        iCount xs = toInteger (length (filter (== 'i') xs))


------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки???. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 str = parse str
    where
        parse :: String -> Maybe String
        parse input = do
            let leftN =  read $ takeWhile (/= '-') input
            let rightM =  read $ takeWhile (/= ':') $ tail $ dropWhile (/= '-') input
            let parseStr = tail $ dropWhile (/= ' ') input
            result leftN rightM parseStr



result :: Int -> Int -> String -> Maybe String
result leftN rightM parseString
    | leftN > (length parseString)  || rightM > (length parseString) = Nothing
    | leftN > rightM = (Just (reverseStr (take leftN  $ drop (rightM - 1) parseString)))
    | otherwise = (Just (take rightM  $ drop (leftN - 1) parseString))


reverseStr :: [a] -> [a]
reverseStr = foldl (\acc x -> x : acc) []

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 n = isPerfect (8 * n + 1)
             where isPerfect m = r * r == m
                    where r = floor . sqrt $ fromIntegral m
-- Число треугольное, если 8 * n + 1 идеальный квадрат, потому что
-- n = k * (k + 1) / 2 -> k^2 + k + 2n = 0 -> k1,k2 = (-1 +- sqrt(D) ) / 2

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 x = reversal x == x

reversal :: Integral a => a -> a
reversal = go 0
  where
    go a 0 = a
    go a b = let (q, r) = b `quotRem` 10 in go (a * 10 + r) q


------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
--prob26 x y = getPrimeDivisors 2
--    where
--        sumDivisors :: Integer -> Integer
--        sumDivisors a = sum (removeItem a (1 : getPrimeDivisors 2 a))
prob26 x y = sum (getAllDivisors x) == y && sum (getAllDivisors y) == x


------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 _ [] = Nothing
prob27 requiredSum (curHead : curTail) = withFixedCurrent curHead curTail
    where
        withFixedCurrent :: Int -> [Int] -> Maybe (Int, Int)
        withFixedCurrent _ [] = prob27 requiredSum curTail
        withFixedCurrent current (innerHead : innerTail) =
            if current + innerHead == requiredSum
            then Just (current, innerHead)
            else withFixedCurrent current innerTail

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 kLength = maximum [(x * y) |
    x <- [minByLength .. maxByLength],
    y <- [minByLength .. maxByLength],
    (prob25 . toInteger) (x * y)]
    where
        minByLength = 10 ^ (kLength - 1)
        maxByLength = 10 ^ kLength - 1

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 reqCount = head $
    filter (\triangular -> (length . getAllDivisors) triangular >= reqCount - 1)
    triangleNumbers

-- Бесконечный список треугольных чисел.
triangleNumbers :: [Integer]
triangleNumbers = map (\n -> n * (n + 1) `div` 2) [0..]

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 maxValue = sum $ map (\(left, right) -> left + right) amicablePairs
    where
        amicablePairs :: [(Int, Int)]
        amicablePairs = concat $ map getAmicablePair [1 .. pred maxValue]

        getAmicablePair :: Int -> [(Int, Int)]
        getAmicablePair leftNumber =
            let amicableNumberValue = divisorsSum leftNumber
            in checkValue leftNumber amicableNumberValue
                where
                    checkValue :: Int -> Int -> [(Int, Int)]
                    checkValue leftNumber amicableNumberValue
                        | leftNumber < amicableNumberValue
                            && leftNumber == divisorsSum amicableNumberValue
                            && amicableNumberValue < maxValue = [(leftNumber, amicableNumberValue)]
                        | otherwise = []

        divisorsSum :: Int -> Int
        divisorsSum = sum . getAllDivisors

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 coins moneySum
    | moneySum < minimum coins = []
    | otherwise = [coin : nextCoins |
        coin <- reverse coins,
        nextCoins <- [] : prob32 (filter (<= coin) coins) (moneySum - coin),
        sum (coin : nextCoins) == moneySum]

-- Пример с 2, 3, 5 и суммой 10
-- 5, 5
-- 5, 3, 2
-- 3, 3, 2, 2
-- 2, 2, 2, 2

